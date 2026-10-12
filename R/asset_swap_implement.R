# TONAR OIS: 2026-10-05 through 2026-10-09; editable provisional inputs.
# Edit rate_pct below: 2.38 means 2.38%. rate is recomputed automatically.
# Oct 5-8: BlueGamma public closes plus spline estimates where missing.
# Oct 9: JSCC settlement rates. Mixed sources and valuation cuts.
# Estimated rows are marked in comments; original provenance remains in
# TONAR_last_week_20261005_09_sources.csv. Manual edits supersede that snapshot.
# JSCC prohibits external redistribution of its published rates; local use.

Glumgamma <- tibble::tribble(
  ~date,        ~tenor, ~rate_pct,
  "2026-10-05", "1M", 1.23000000, # BlueGamma
  "2026-10-05", "3M", 1.29000000, # BlueGamma
  "2026-10-05", "6M", 1.42000000, # BlueGamma
  "2026-10-05", "1Y", 1.64000000, # BlueGamma
  "2026-10-05", "2Y", 1.91000000, # BlueGamma
  "2026-10-05", "3Y", 2.09000000, # BlueGamma
  "2026-10-05", "4Y", 2.24000000, # BlueGamma
  "2026-10-05", "5Y", 2.38000000, # BlueGamma
  "2026-10-05", "7Y", 2.63483926, # spline estimate (interpolated)
  "2026-10-05", "8Y", 2.74899841, # spline estimate (interpolated)
  "2026-10-05", "10Y", 2.95000000, # BlueGamma
  "2026-10-05", "15Y", 3.30856764, # spline estimate (interpolated)
  "2026-10-05", "20Y", 3.55000000, # BlueGamma
  "2026-10-05", "30Y", 3.99381962, # spline estimate (extrapolated)
  "2026-10-06", "1M", 1.23000000, # BlueGamma
  "2026-10-06", "3M", 1.29000000, # BlueGamma
  "2026-10-06", "6M", 1.42000000, # BlueGamma
  "2026-10-06", "1Y", 1.65000000, # BlueGamma
  "2026-10-06", "2Y", 1.93000000, # BlueGamma
  "2026-10-06", "3Y", 2.11000000, # BlueGamma
  "2026-10-06", "4Y", 2.26000000, # BlueGamma
  "2026-10-06", "5Y", 2.40000000, # BlueGamma
  "2026-10-06", "7Y", 2.65197582, # spline estimate (interpolated)
  "2026-10-06", "8Y", 2.76376531, # spline estimate (interpolated)
  "2026-10-06", "10Y", 2.96000000, # BlueGamma
  "2026-10-06", "15Y", 3.31477453, # spline estimate (interpolated)
  "2026-10-06", "20Y", 3.56000000, # BlueGamma
  "2026-10-06", "30Y", 4.01393458, # spline estimate (extrapolated)
  "2026-10-07", "1M", 1.23000000, # BlueGamma
  "2026-10-07", "3M", 1.30000000, # BlueGamma
  "2026-10-07", "6M", 1.43000000, # BlueGamma
  "2026-10-07", "1Y", 1.65830337, # spline estimate (interpolated)
  "2026-10-07", "2Y", 1.95000000, # BlueGamma
  "2026-10-07", "3Y", 2.13000000, # BlueGamma
  "2026-10-07", "4Y", 2.29000000, # BlueGamma
  "2026-10-07", "5Y", 2.42000000, # BlueGamma
  "2026-10-07", "7Y", 2.65980692, # spline estimate (interpolated)
  "2026-10-07", "8Y", 2.77376183, # spline estimate (interpolated)
  "2026-10-07", "10Y", 2.98000000, # BlueGamma
  "2026-10-07", "15Y", 3.34051302, # spline estimate (interpolated)
  "2026-10-07", "20Y", 3.57000000, # BlueGamma
  "2026-10-07", "30Y", 3.98529862, # spline estimate (extrapolated)
  "2026-10-08", "1M", 1.24000000, # BlueGamma
  "2026-10-08", "3M", 1.30000000, # BlueGamma
  "2026-10-08", "6M", 1.44000000, # BlueGamma
  "2026-10-08", "1Y", 1.66000000, # BlueGamma
  "2026-10-08", "2Y", 1.94000000, # BlueGamma
  "2026-10-08", "3Y", 2.12000000, # BlueGamma
  "2026-10-08", "4Y", 2.28000000, # BlueGamma
  "2026-10-08", "5Y", 2.41000000, # BlueGamma
  "2026-10-08", "7Y", 2.65000000, # BlueGamma
  "2026-10-08", "8Y", 2.76000000, # BlueGamma
  "2026-10-08", "10Y", 2.97000000, # BlueGamma
  "2026-10-08", "15Y", 3.34000000, # BlueGamma
  "2026-10-08", "20Y", 3.56000000, # BlueGamma
  "2026-10-08", "30Y", 3.70000000, # BlueGamma
  "2026-10-09", "1M", 1.23780000, # JSCC
  "2026-10-09", "3M", 1.29910000, # JSCC
  "2026-10-09", "6M", 1.42500000, # JSCC
  "2026-10-09", "1Y", 1.64375000, # JSCC
  "2026-10-09", "2Y", 1.91188000, # JSCC
  "2026-10-09", "3Y", 2.08375000, # JSCC
  "2026-10-09", "4Y", 2.22438000, # JSCC
  "2026-10-09", "5Y", 2.35188000, # JSCC
  "2026-10-09", "7Y", 2.58438000, # JSCC
  "2026-10-09", "8Y", 2.69438000, # JSCC
  "2026-10-09", "10Y", 2.89428000, # JSCC
  "2026-10-09", "15Y", 3.26467000, # JSCC
  "2026-10-09", "20Y", 3.48963000, # JSCC
  "2026-10-09", "30Y", 3.63810000 # JSCC
)
Glumgamma$date <- as.Date(Glumgamma$date)
Glumgamma$rate <- Glumgamma$rate_pct / 100
tonar_last_week <- Glumgamma

# Example: Glumgamma[Glumgamma$date == as.Date("2026-10-09"), ]

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

tonar_asset_swap_summary_GQL <- function(asset_swap) {
  stopifnot(inherits(asset_swap, "tonar_asset_swap_GQL"))
  asset_swap$valuation
}

# Usage: tonar <- instance_swap_curve_TONAR("2026-10-09")
instance_swap_curve_TONAR <- function(date = "2026-10-09") {
  date <- as.Date(date)
  if (length(date) != 1L || is.na(date)) stop("Choose one date.")
  if (date == as.Date("2022-08-19")) return(instance_swap_curve_TONAR_20220819())
  q <- Glumgamma[Glumgamma$date == date, ]
  if (!nrow(q)) stop("No TONAR data for this date.")
  if (any(!is.finite(q$rate_pct)) || anyDuplicated(q$tenor))
    stop("Check TONAR rates and tenors.")
  quotes <- tibble::tibble(
    quote_id = paste0("TONAR-",q$tenor), instrument_type = "spot_ois",
    rate_pct = q$rate_pct, rate = q$rate_pct/100,
    fixing_days = rep(2L,nrow(q)),
    tenor_n = as.integer(sub("[MY]$","",q$tenor)),
    tenor_unit = ifelse(grepl("M$",q$tenor),"Months","Years"))
  .tasw_gq_GQH("build_tonar_curve_from_market_GQL",
               quotes=quotes,evaluation_date=as.character(date),
               curve_type="log_cubic_discount")
}

# Existing bullet JPY bond; clean price per 100, spread in bp.
compute_asset_swap <- function(bond, clean_price, tonar = instance_swap_curve_TONAR(), spread_bp = 0,
                             payment_frequency = "Semiannual") {
  .tasw_gq_GQH("set_eval_date_GQL",.tasw_date_GQH(tonar$evaluation_date))
  settlement <- .tasw_ql_GQH("Bond_settlementDate",bond)
  effective <- as.Date(.tasw_gq_GQH("safe_iso_GQH",settlement))
  make_tonar_asset_swap_GQL(
    bond=bond,clean_price=clean_price,
    forecast_handle=tonar$curve_handle,discount_curve=tonar$curve,
    effective_date=effective,spread=spread_bp/10000,
    payment_frequency=payment_frequency)
}

# Historical inputs supplied by the user; first quote is a TONAR deposit.
instance_swap_curve_TONAR_20220819 <- function() {
  .tasw_gq_GQH("set_eval_date_GQL",.tasw_date_GQH("2022-08-19"))
  cal <- .tasw_ql_GQH("Japan")
  dc <- .tasw_ql_GQH("Actual365Fixed")
  pct <- c(-0.009,-0.01807,-0.01043,0.0125,0.03125,
           0.04875,0.07375,0.11854,0.19146)
  tenors <- c("1M","6M","12M","18M","2Y","3Y","5Y","7Y")
  index <- .tasw_gq_GQH("tonar_GQL")
  helpers <- list(.tasw_ql_GQH("DepositRateHelper",
    .tasw_gq_GQH("quote_handle_GQL",pct[1]/100),index))
  for (i in seq_along(tenors)) {
    period <- .tasw_ql_GQH("Period",as.integer(sub("[MY]$","",tenors[i])),
      if (grepl("M$",tenors[i])) "Months" else "Years")
    helpers[[i+1L]] <- .tasw_ql_GQH("OISRateHelper",2L,period,
      .tasw_gq_GQH("quote_handle_GQL",pct[i+1L]/100),index)
  }
  vector <- .tasw_gq_GQH("push_rate_helpers_GQL",helpers)
  curve <- .tasw_ql_GQH("PiecewiseLogCubicDiscount",0L,cal,vector,dc)
  list(curve=curve,curve_handle=.tasw_ql_GQH("YieldTermStructureHandle",curve),
       evaluation_date="2022-08-19",helpers=helpers,helper_vector=vector)
}

# tonar <- instance_swap_curve_TONAR("2026-10-09")
# asw <- compute_asset_swap(bond, 99.5, tonar)
# asw$valuation
# asw$cashflows




