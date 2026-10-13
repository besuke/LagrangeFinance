Glumgamma <- tibble::tribble(
  ~date, ~tenor, ~rate_pct,
  "2026-10-05", "1M", 1.23000000,
  "2026-10-05", "3M", 1.29000000,
  "2026-10-05", "6M", 1.42000000,
  "2026-10-05", "1Y", 1.64000000,
  "2026-10-05", "2Y", 1.91000000,
  "2026-10-05", "3Y", 2.09000000,
  "2026-10-05", "4Y", 2.24000000,
  "2026-10-05", "5Y", 2.38000000,
  "2026-10-05", "7Y", 2.63483926,
  "2026-10-05", "8Y", 2.74899841,
  "2026-10-05", "10Y", 2.95000000,
  "2026-10-05", "15Y", 3.30856764,
  "2026-10-05", "20Y", 3.55000000,
  "2026-10-05", "30Y", 3.99381962,
  "2026-10-06", "1M", 1.23000000,
  "2026-10-06", "3M", 1.29000000,
  "2026-10-06", "6M", 1.42000000,
  "2026-10-06", "1Y", 1.65000000,
  "2026-10-06", "2Y", 1.93000000,
  "2026-10-06", "3Y", 2.11000000,
  "2026-10-06", "4Y", 2.26000000,
  "2026-10-06", "5Y", 2.40000000,
  "2026-10-06", "7Y", 2.65197582,
  "2026-10-06", "8Y", 2.76376531,
  "2026-10-06", "10Y", 2.96000000,
  "2026-10-06", "15Y", 3.31477453,
  "2026-10-06", "20Y", 3.56000000,
  "2026-10-06", "30Y", 4.01393458,
  "2026-10-07", "1M", 1.23000000,
  "2026-10-07", "3M", 1.30000000,
  "2026-10-07", "6M", 1.43000000,
  "2026-10-07", "1Y", 1.65830337,
  "2026-10-07", "2Y", 1.95000000,
  "2026-10-07", "3Y", 2.13000000,
  "2026-10-07", "4Y", 2.29000000,
  "2026-10-07", "5Y", 2.42000000,
  "2026-10-07", "7Y", 2.65980692,
  "2026-10-07", "8Y", 2.77376183,
  "2026-10-07", "10Y", 2.98000000,
  "2026-10-07", "15Y", 3.34051302,
  "2026-10-07", "20Y", 3.57000000,
  "2026-10-07", "30Y", 3.98529862,
  "2026-10-08", "1M", 1.24000000,
  "2026-10-08", "3M", 1.30000000,
  "2026-10-08", "6M", 1.44000000,
  "2026-10-08", "1Y", 1.66000000,
  "2026-10-08", "2Y", 1.94000000,
  "2026-10-08", "3Y", 2.12000000,
  "2026-10-08", "4Y", 2.28000000,
  "2026-10-08", "5Y", 2.41000000,
  "2026-10-08", "7Y", 2.65000000,
  "2026-10-08", "8Y", 2.76000000,
  "2026-10-08", "10Y", 2.97000000,
  "2026-10-08", "15Y", 3.34000000,
  "2026-10-08", "20Y", 3.56000000,
  "2026-10-08", "30Y", 3.70000000,
  "2026-10-09", "1M", 1.23780000,
  "2026-10-09", "3M", 1.29910000,
  "2026-10-09", "6M", 1.42500000,
  "2026-10-09", "1Y", 1.64375000,
  "2026-10-09", "2Y", 1.91188000,
  "2026-10-09", "3Y", 2.08375000,
  "2026-10-09", "4Y", 2.22438000,
  "2026-10-09", "5Y", 2.35188000,
  "2026-10-09", "7Y", 2.58438000,
  "2026-10-09", "8Y", 2.69438000,
  "2026-10-09", "10Y", 2.89428000,
  "2026-10-09", "15Y", 3.26467000,
  "2026-10-09", "20Y", 3.48963000,
  "2026-10-09", "30Y", 3.63810000
)
Glumgamma$date <- as.Date(Glumgamma$date)
Glumgamma$rate <- Glumgamma$rate_pct / 100



instance_swap_curve_TONAR <- function(date = "2026-10-09") {
  date <- as.Date(date)
  if (length(date) != 1L || is.na(date)) stop("Choose one date.")
  if (date == as.Date("2022-08-19")) {
    return(instance_swap_curve_TONAR_20220819())
  }
  q <- Glumgamma[Glumgamma$date == date, ]
  if (!nrow(q)) stop("No TONAR data for this date.")
  if (any(!is.finite(q$rate_pct)) || anyDuplicated(q$tenor)) {
    stop("Check TONAR rates and tenors.")
  }
  quotes <- tibble::tibble(
    quote_id = paste0("TONAR-", q$tenor), instrument_type = "spot_ois",
    rate_pct = q$rate_pct, rate = q$rate_pct / 100,
    fixing_days = rep(2L, nrow(q)),
    tenor_n = as.integer(sub("[MY]$", "", q$tenor)),
    tenor_unit = ifelse(grepl("M$", q$tenor), "Months", "Years")
  )
  bundle <- .tasw_gq_GQH("build_tonar_curve_from_market_GQL",
    quotes = quotes, evaluation_date = as.character(date),
    curve_type = "log_cubic_discount"
  )
  bundle$overnight_index <- .tasw_ql_GQH("Tonar", bundle$curve_handle)
  bundle
}


instance_swap_curve_TONAR_20220819 <- function() {
  .tasw_gq_GQH("set_eval_date_GQL", .tasw_date_GQH("2022-08-19"))
  cal <- .tasw_ql_GQH("Japan")
  dc <- .tasw_ql_GQH("Actual365Fixed")
  pct <- c(
    -0.009, -0.01807, -0.01043, 0.0125, 0.03125,
    0.04875, 0.07375, 0.11854, 0.19146
  )
  tenors <- c("1M", "6M", "12M", "18M", "2Y", "3Y", "5Y", "7Y")
  index <- .tasw_gq_GQH("tonar_GQL")
  helpers <- list(.tasw_ql_GQH(
    "DepositRateHelper",
    .tasw_gq_GQH("quote_handle_GQL", pct[1] / 100), index
  ))
  for (i in seq_along(tenors)) {
    period <- .tasw_ql_GQH(
      "Period", as.integer(sub("[MY]$", "", tenors[i])),
      if (grepl("M$", tenors[i])) "Months" else "Years"
    )
    helpers[[i + 1L]] <- .tasw_ql_GQH(
      "OISRateHelper", 2L, period,
      .tasw_gq_GQH("quote_handle_GQL", pct[i + 1L] / 100), index
    )
  }
  vector <- .tasw_gq_GQH("push_rate_helpers_GQL", helpers)
  curve <- .tasw_ql_GQH("PiecewiseLogCubicDiscount", 0L, cal, vector, dc)
  bundle <- list(
    curve = curve, curve_handle = .tasw_ql_GQH("YieldTermStructureHandle", curve),
    evaluation_date = "2022-08-19", helpers = helpers, helper_vector = vector
  )
  bundle$overnight_index <- .tasw_ql_GQH("Tonar", bundle$curve_handle)
  bundle
}
