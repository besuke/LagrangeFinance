# TONA par asset swap: pay bond cashflows, receive compounded TONA + spread.
# This is a cashflow valuation bundle, not a QuantLib AssetSwap instance.
# Supported: non-amortising, non-callable, non-ex-coupon FixedRateBond,
# redemption at 100, settlement on the OIS effective date.
# Source this file alongside jscc_tonar_quotes_GQL.R, or add both to GaussQuant/R.

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

#' Build a TONA curve from the latest JSCC settlement quotes
#' @param quotes Optional quotes; NULL downloads the latest published quotes.
#' @param curve_type GaussQuant curve interpolation choice.
#' @return Existing GaussQuant curve bundle, with source quotes and date.
#' @export
jscc_tonar_curve_GQL <- function(quotes = NULL, curve_type = "log_cubic_discount") {
  if (is.null(quotes)) quotes <- jscc_tonar_quotes_GQL()
  dates <- unique(as.Date(quotes$quote_date))
  if (length(dates) != 1L || anyNA(dates)) {
    stop("Quotes must have one publication date.", call. = FALSE)
  }
  .tasw_gq_GQH("build_tonar_curve_from_market_GQL",
    quotes = quotes, evaluation_date = as.character(dates), curve_type = curve_type)
}

#' Construct a TONA par asset-swap cashflow valuation bundle
#'
#' The bond holder pays the remaining bond coupons and redemption, receives
#' TONA plus a simple additive spread and par principal, and receives
#' (dirty price - 100) * nominal / 100 upfront. A negative upfront is paid.
#' Historical TONA fixings must already be loaded into QuantLib when required.
#' The evaluation date must be set before calling this function.
#' No assumption about collateral is made: caller supplies the discount curve.
#'
#' @param bond Bullet FixedRateBond with redemption 100, no ex-coupon features.
#' @param clean_price Market price per 100 of face value.
#' @param forecast_handle QuantLib TONA forecasting handle.
#' @param discount_curve QuantLib yield curve exposing discount(Date).
#' @param effective_date Bond settlement date / swap start (ISO or Date).
#' @param spread Decimal additive spread, e.g. 0.001 for 10 bp.
#' @param payment_frequency QuantLib Frequency; default Semiannual.
#' @param payment_lag Business-day payment lag; default 0 (explicit trade choice).
#' @return A tonar_asset_swap_GQL list with signed cashflows and valuation.
#' @export
make_tonar_asset_swap_GQL <- function(
  bond, clean_price, forecast_handle, discount_curve, effective_date,
  spread = 0, payment_frequency = "Semiannual", payment_lag = 0L
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
  # Last cashflow must be par redemption; remaining notionals must be constant.
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
  index <- .tasw_ql_GQH("Tonar", forecast_handle)
  frequency <- if (is.character(payment_frequency)) {
    if (length(payment_frequency) != 1L ||
        !payment_frequency %in% c("Annual", "Semiannual", "Quarterly", "Monthly")) {
      stop("Unsupported payment_frequency.", call. = FALSE)
    }
    .tasw_ql_GQH(paste0("Frequency_", payment_frequency, "_get"))
  } else payment_frequency
  builder <- .tasw_ql_GQH("MakeOIS",
    .tasw_ql_GQH("Period", 1L, "Years"), index, 1)
  settings <- list(
    withEffectiveDate = start_ql,
    withTerminationDate = .tasw_date_GQH(maturity),
    withNominal = nominal,
    withPaymentFrequency = frequency,
    withPaymentLag = as.integer(payment_lag),
    withFixedLegDayCount = .tasw_ql_GQH("Actual365Fixed")
  )
  builder <- purrr::reduce(names(settings), function(b, name) {
    .tasw_ql_GQH(paste0("MakeOIS_", name), b, settings[[name]])
  }, .init = builder)
  ois <- .tasw_ql_GQH("MakeOIS_makeOIS", builder)
  float <- .tasw_table_GQH(ois$overnightLeg())
  # Unit-rate fixed coupons provide N * accrual fraction on the same schedule.
  unit <- .tasw_table_GQH(ois$fixedLeg())
  if (!identical(float$payment_date, unit$payment_date) || any(unit$amount <= 0)) {
    stop("Fixed/overnight schedules differ; cannot compute spread annuity.", call. = FALSE)
  }
  float_df <- .tasw_discount_GQH(discount_curve, float$payment_date)
  annuity <- sum(unit$amount * float_df)
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
    tibble::tibble(leg = "receive_tona", payment_date = float$payment_date,
                  amount = float$amount + spread * unit$amount),
    tibble::tibble(leg = "receive_principal", payment_date = principal_date, amount = nominal),
    tibble::tibble(leg = "upfront", payment_date = start, amount = upfront)
  ) |>
    dplyr::mutate(discount_factor = .tasw_discount_GQH(discount_curve, payment_date),
                  present_value = amount * discount_factor)
  structure(list(
    cashflows = signed_cf, bond = bond, ois = ois, index = index,
    forecast_handle = forecast_handle, discount_curve = discount_curve,
    valuation = tibble::tibble(
      clean_price = clean_price, accrued_per_100 = accrued, dirty_price = dirty,
      nominal = nominal, spread = spread, fair_spread = fair_spread,
      fair_spread_bp = fair_spread * 1e4, spread_annuity = annuity,
      npv = npv_zero + spread * annuity,
      bond_leg_pv = -bond_pv, tona_leg_pv = float_pv + spread * annuity,
      principal_pv = principal_pv, upfront_pv = upfront_pv),
    effective_date = start, maturity_date = maturity,
    payment_frequency = payment_frequency, payment_lag = payment_lag
  ), class = "tonar_asset_swap_GQL")
}

#' Summarise a TONA asset-swap valuation bundle
#' @param asset_swap Result from make_tonar_asset_swap_GQL().
#' @return One-row valuation tibble.
#' @export
tonar_asset_swap_summary_GQL <- function(asset_swap) {
  stopifnot(inherits(asset_swap, "tonar_asset_swap_GQL"))
  asset_swap$valuation
}

# Example (bond is an existing eligible JPY FixedRateBond object):
# curve_bundle <- jscc_tonar_curve_GQL()
# asw <- make_tonar_asset_swap_GQL(
#   bond = bond, clean_price = 99.5,
#   forecast_handle = curve_bundle$curve_handle,
#   discount_curve = curve_bundle$curve,
#   effective_date = "2026-10-14", spread = 0.001,
#   payment_frequency = "Semiannual", payment_lag = 0L
# )
# tonar_asset_swap_summary_GQL(asw)
# asw$cashflows
# Before package integration, validate available SWIG bindings and reprice
# at fair_spread (NPV should be zero within numerical tolerance).

