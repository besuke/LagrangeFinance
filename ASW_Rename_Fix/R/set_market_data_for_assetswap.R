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




# Adapt the retained TONAR data to the same quote schema as the Excel reader.
# No date relabelling: 2026-10-06 remains 2026-10-06.
tonar_market_quotes <- function(marketdate, quote = c("mid", "bid", "ask")) {
  quote <- match.arg(quote)
  marketdate <- as.Date(marketdate)
  if (length(marketdate) != 1L || is.na(marketdate)) stop("Choose one marketdate.")
  if (quote != "mid") stop("Legacy TONAR data has mid quotes only.")
  if (marketdate == as.Date("2022-08-19")) {
    q <- tibble::tibble(
      date = marketdate,
      tenor = c("1D", "1M", "6M", "12M", "18M", "2Y", "3Y", "5Y", "7Y"),
      rate_pct = c(-.009, -.01807, -.01043, .0125, .03125,
                   .04875, .07375, .11854, .19146),
      instrument_type = c("deposit", rep("spot_ois", 8))
    )
  } else {
    q <- Glumgamma |> dplyr::filter(.data$date == .env$marketdate) |>
      dplyr::mutate(instrument_type = "spot_ois")
  }
  if (!nrow(q)) stop("No legacy TONAR data for marketdate.")
  q |> dplyr::transmute(
    marketdate = .data$date, currency = "JPY", tenor,
    ticker = NA_character_, instrument = "TONAR_OIS", instrument_type,
    tenordate = as.Date(NA), ask = NA_real_, mid = rate_pct, bid = NA_real_,
    rate_pct, rate = rate_pct / 100,
    source = "Retained TONAR data in set_market_data_for_assetswap.R"
  )
}

# Backward-compatible names; curve construction has one shared implementation.
instance_swap_curve_TONAR <- function(date = "2026-10-09", ...) {
  instance_swap_curve("JPY", marketdate = date,
                      quotes = tonar_market_quotes(date), ...)
}
instance_swap_curve_TONAR_20220819 <- function(...) {
  instance_swap_curve_TONAR("2022-08-19", ...)
}


