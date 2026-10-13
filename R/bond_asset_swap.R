.tasw_ql_GQH <- function(name, ...) {
  ns <- asNamespace("QuantLib")
  if (!exists(name, ns, inherits = FALSE)) {
    stop("Required QuantLib R binding is unavailable: ", name, call. = FALSE)
  }
  get(name, ns, inherits = FALSE)(...)
}

.tasw_gq_GQH <- function(name, ...) {
  get(name, asNamespace("GaussQuant"), inherits = FALSE)(...)
}

.tasw_date_GQH <- function(x) {
  if (is.character(x) || inherits(x, "Date")) {
    return(.tasw_gq_GQH("date_GQL", as.character(x)))
  }
  x
}

.tasw_table_GQH <- function(leg) {
  out <- .tasw_gq_GQH("leg_to_cashflow_tbl_GQL", leg)
  out <- .tasw_gq_GQH("cashflow_standardise_GQL", out)
  if (anyNA(out$payment_date) || any(!is.finite(out$amount))) {
    stop("Invalid cashflow dates or amounts.", call. = FALSE)
  }
  out
}

.tasw_discount_GQH <- function(curve, dates) {
  purrr::map_dbl(dates, function(d) {
    value <- as.numeric(curve$discount(.tasw_date_GQH(as.Date(d, origin = "1970-01-01"))))
    if (length(value) != 1L || !is.finite(value) || value <= 0) {
      stop("Invalid discount factor.", call. = FALSE)
    }
    value
  })
}

make_tonar_asset_swap_GQL <- function(
  bond, clean_price, forecast_handle, discount_curve, effective_date,
  spread = 0, payment_frequency = "Semiannual", payment_lag = 0L,
  index = NULL, spread_day_counter = NULL
) {
  if (length(clean_price) != 1L || !is.numeric(clean_price) ||
    !is.finite(clean_price) || clean_price <= 0 || length(spread) != 1L ||
    !is.numeric(spread) || !is.finite(spread)) {
    stop("clean_price must be positive and spread must be finite.", call. = FALSE)
  }
  if (length(payment_lag) != 1L || !is.numeric(payment_lag) ||
    is.na(payment_lag) || payment_lag < 0 || payment_lag != as.integer(payment_lag)) {
    stop("payment_lag must be a non-negative integer.", call. = FALSE)
  }
  start <- as.Date(effective_date)
  if (length(start) != 1L || is.na(start)) stop("Invalid effective_date.", call. = FALSE)
  start_ql <- .tasw_date_GQH(start)
  eval_raw <- .tasw_gq_GQH("eval_date_get_GQL")
  eval <- if (inherits(eval_raw, "Date") || is.character(eval_raw)) {
    as.Date(eval_raw)
  } else {
    as.Date(.tasw_gq_GQH("safe_iso_GQH", eval_raw))
  }
  if (length(eval) != 1L || is.na(eval) || start < eval) {
    stop("Effective date must be on or after the QuantLib evaluation date.", call. = FALSE)
  }
  cf <- .tasw_table_GQH(.tasw_ql_GQH("Bond_cashflows", bond)) |>
    dplyr::filter(payment_date > start)
  if (!nrow(cf)) stop("No remaining bond cashflows.", call. = FALSE)
  maturity <- as.Date(.tasw_gq_GQH("safe_iso_GQH", .tasw_ql_GQH("Bond_maturityDate", bond)))
  nominal <- as.numeric(.tasw_ql_GQH("Bond_notional", bond, start_ql))
  if (!is.finite(nominal) || nominal <= 0 || is.na(maturity) || maturity <= start) {
    stop("Invalid bond nominal or maturity.", call. = FALSE)
  }
  if (abs(tail(cf$amount, 1L) - nominal) > 1e-8 * nominal) {
    stop("Only bullet bonds with redemption at 100 are supported.", call. = FALSE)
  }
  before_dates <- unique(cf$payment_date[cf$payment_date < maturity])
  notionals <- purrr::map_dbl(before_dates, function(d) {
    as.numeric(.tasw_ql_GQH("Bond_notional", bond, .tasw_date_GQH(as.Date(d, origin = "1970-01-01"))))
  })
  if (any(!is.finite(notionals)) || any(abs(notionals - nominal) > 1e-8 * nominal)) {
    stop("Amortising bonds are not supported.", call. = FALSE)
  }
  accrued <- as.numeric(.tasw_ql_GQH("Bond_accruedAmount", bond, start_ql))
  if (!is.finite(accrued)) stop("Invalid accrued interest.", call. = FALSE)
  dirty <- clean_price + accrued
  if (is.null(index)) index <- .tasw_ql_GQH("Tonar", forecast_handle)
  if (is.null(spread_day_counter)) spread_day_counter <- index$dayCounter()
  frequency <- if (is.character(payment_frequency)) {
    if (length(payment_frequency) != 1L ||
      !payment_frequency %in% c("Annual", "Semiannual", "Quarterly", "Monthly")) {
      stop("Unsupported payment_frequency.", call. = FALSE)
    }
    .tasw_ql_GQH(paste0("Frequency_", payment_frequency, "_get"))
  } else {
    payment_frequency
  }
  builder <- .tasw_ql_GQH(
    "MakeOIS",
    .tasw_ql_GQH("Period", 1L, "Years"), index, 1
  )
  settings <- list(
    withEffectiveDate = start_ql,
    withTerminationDate = .tasw_date_GQH(maturity),
    withNominal = nominal,
    withPaymentFrequency = frequency,
    withPaymentLag = as.integer(payment_lag),
    withFixedLegDayCount = spread_day_counter
  )
  builder <- purrr::reduce(names(settings), function(b, name) {
    .tasw_ql_GQH(paste0("MakeOIS_", name), b, settings[[name]])
  }, .init = builder)
  ois <- .tasw_ql_GQH("MakeOIS_makeOIS", builder)
  float <- .tasw_table_GQH(ois$overnightLeg())
  unit <- .tasw_table_GQH(ois$fixedLeg())
  if (!identical(float$payment_date, unit$payment_date) || any(unit$amount <= 0)) {
    stop("Fixed/overnight schedules differ; cannot compute spread annuity.", call. = FALSE)
  }
  float_df <- .tasw_discount_GQH(discount_curve, float$payment_date)
  annuity <- sum(unit$amount * float_df)
  if (!is.finite(annuity) || annuity <= 0) stop("Invalid spread annuity.")
  principal_date <- tail(float$payment_date, 1L)
  bond_pv <- sum(cf$amount * .tasw_discount_GQH(discount_curve, cf$payment_date))
  float_pv <- sum(float$amount * float_df)
  principal_pv <- nominal * .tasw_discount_GQH(discount_curve, principal_date)
  upfront <- nominal * (dirty / 100 - 1)
  upfront_pv <- upfront * .tasw_discount_GQH(discount_curve, start)
  npv_zero <- float_pv + principal_pv + upfront_pv - bond_pv
  fair_spread <- -npv_zero / annuity
  signed_cf <- dplyr::bind_rows(
    tibble::tibble(leg = "pay_bond", payment_date = cf$payment_date, amount = -cf$amount),
    tibble::tibble(
      leg = "receive_tona", payment_date = float$payment_date,
      amount = float$amount + spread * unit$amount
    ),
    tibble::tibble(leg = "receive_principal", payment_date = principal_date, amount = nominal),
    tibble::tibble(leg = "upfront", payment_date = start, amount = upfront)
  ) |>
    dplyr::mutate(
      discount_factor = .tasw_discount_GQH(discount_curve, payment_date),
      present_value = amount * discount_factor
    )
  structure(list(
    cashflows = signed_cf, bond = bond, ois = ois, index = index,
    forecast_handle = forecast_handle, discount_curve = discount_curve,
    valuation = tibble::tibble(
      clean_price = clean_price, accrued_per_100 = accrued, dirty_price = dirty,
      nominal = nominal, spread = spread, fair_spread = fair_spread,
      fair_spread_bp = fair_spread * 1e4, spread_annuity = annuity,
      npv = npv_zero + spread * annuity,
      bond_leg_pv = -bond_pv, tona_leg_pv = float_pv + spread * annuity,
      principal_pv = principal_pv, upfront_pv = upfront_pv
    ),
    effective_date = start, maturity_date = maturity,
    payment_frequency = payment_frequency, payment_lag = payment_lag
  ), class = "tonar_asset_swap_GQL")
}

tonar_asset_swap_summary_GQL <- function(asset_swap) {
  stopifnot(inherits(asset_swap, "tonar_asset_swap_GQL"))
  asset_swap$valuation
}

compute_asset_swap_TONAR <- function(bond, clean_price, swap_curve, spread_bp = 0,
                                     payment_frequency = "Semiannual",
                                     index = swap_curve$overnight_index,
                                     discount_curve = swap_curve$curve) {
  if (length(spread_bp) != 1L || !is.numeric(spread_bp) || !is.finite(spread_bp)) {
    stop("spread_bp must be one finite number.")
  }
  if (is.null(index)) stop("Supply a linked overnight index, e.g. TONAR or SOFR.")
  .tasw_gq_GQH("set_eval_date_GQL", .tasw_date_GQH(swap_curve$evaluation_date))
  settlement <- .tasw_ql_GQH("Bond_settlementDate", bond)
  effective <- as.Date(.tasw_gq_GQH("safe_iso_GQH", settlement))
  out <- make_tonar_asset_swap_GQL(
    bond = bond, clean_price = clean_price,
    forecast_handle = swap_curve$curve_handle, discount_curve = discount_curve,
    effective_date = effective, spread = spread_bp / 10000,
    payment_frequency = payment_frequency, index = index
  )
  class(out) <- c("asset_swap_valuation", class(out))
  out
}
