# Unified executable sample. Run from the LagrangeFinance project root.
# Switch sections independently. ALL bonds / FX / CCS basis and demo offers are synthetic.
run_asset_swap <- TRUE
run_worksheet_demo <- TRUE
run_historical <- FALSE  # retained 2022-08-19 benchmark
run_ccs_checks <- FALSE  # retained deterministic PV identity checks

# Unified sample: run this ONE file from the LagrangeFinance project root.
# Market observations: 2026-10-06. Bonds / FX / basis below are test assumptions.
library(GaussQuant)
library(dplyr)
library(tibble)
library(purrr)

purrr::walk(c(
  "import_data.R", "workflows_validate_worksheet.R"
), function(f) source(file.path("R", f)))

# Retained historical and October TONAR fixtures; values unchanged.
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
    source = "Retained TONAR data in sample_bond_asset_swap.R"
  )
}

# Map the quote schema of import_ois_curves() / tonar_market_quotes() to
# GaussQuant::ir_build_ois_curve_GQH(). Conventions: log-cubic discount factors,
# evaluation date as reference, annual OIS helpers, no lag, ModifiedFollowing.
ois_curve_from_quotes <- function(currency, marketdate, quotes,
                                  curve_type = "log_cubic_discount") {
  q <- quotes |>
    dplyr::filter(
      as.Date(.data$marketdate) == as.Date(.env$marketdate),
      toupper(.data$currency) == toupper(.env$currency)
    )
  if (!nrow(q)) stop("No quotes for currency/marketdate.")
  if (!"instrument_type" %in% names(q)) q$instrument_type <- "spot_ois"
  # Historical overnight fixings must not masquerade as 1D OIS quotes.
  if (any(!q$instrument_type %in% c("spot_ois", "deposit"))) {
    stop("Only spot_ois/deposit inputs allowed. Separate overnight fixings and unconfirmed IRS.")
  }
  GaussQuant::ir_build_ois_curve_GQH(
    curve_data = q |>
      dplyr::transmute(
        kind = dplyr::if_else(instrument_type == "deposit", "overnight_deposit", "ois"),
        tenor = toupper(trimws(tenor)),
        rate = rate_pct / 100
      ),
    trade_date = as.character(as.Date(marketdate)),
    currency = currency,
    verbose = FALSE,
    curve_type = curve_type,
    reference = "evaluation",
    payment_frequency = "Annual",
    payment_lag = 0L,
    payment_convention = "ModifiedFollowing",
    max_quote_error_bp = 1e-4
  )
}





if (run_asset_swap) {
marketdate <- as.Date("2026-10-06")
path <- "data/raw/market/Bloomberg_MarketData_20261006.xlsx"

# 1. Import all five currencies from the same Excel workbook.
currencies <- c("JPY", "USD", "EUR", "GBP", "AUD")
tbl_market_quotes <- purrr::map_dfr(
  currencies,
  function(ccy) import_ois_curves(
    path = path,
    currency = ccy,
    marketdate = marketdate,
    quote = "mid"
  )
)

# 2. Do not treat a historical fixing or an unconfirmed AUD IRS as spot OIS.
tbl_excluded_quotes <- tbl_market_quotes |>
  filter(!instrument_type %in% c("spot_ois", "deposit"))
tbl_curve_quotes <- tbl_market_quotes |>
  filter(instrument_type %in% c("spot_ois", "deposit"))
if (nrow(tbl_excluded_quotes)) print(tbl_excluded_quotes, n = Inf)

# 3. Same curve builder, same date and same return fields for every currency.
curves <- currencies |> purrr::set_names() |>
  purrr::map(function(ccy) ois_curve_from_quotes(
    currency = ccy, marketdate = marketdate, quotes = tbl_curve_quotes
  ))
tbl_quote_repricing <- purrr::map_dfr(curves, function(x) {
  x$quote_repricing |> mutate(currency = x$currency, .before = 1)
})
tbl_curve_summary <- purrr::map_dfr(curves, function(x) tibble(
  currency = x$currency, index = x$instrument, marketdate = x$trade_date,
  quotes = nrow(x$quote_repricing), last_tenordate = max(x$quote_repricing$maturity_date),
  max_quote_error_bp = max(abs(x$quote_repricing$quote_error_bp))
))
print(tbl_curve_summary)

# AUD has only short confirmed OIS input in this workbook.
# Its sample bond matures in 6 months; no 10Y AONIA curve is extrapolated.
tbl_bonds <- tibble::tribble(
  ~currency, ~coupon_rate, ~clean_price, ~fx_spot, ~basis_bp, ~maturity_date,
  "JPY", .030, 99.5, 1, 0, "2036-09-30",
  "USD", .050, 99.5, 150, -30, "2036-09-30",
  "EUR", .035, 99.5, 165, -20, "2036-09-30",
  "GBP", .050, 99.5, 195, -25, "2036-09-30",
  "AUD", .048, 99.5, 100, -15, "2027-03-30"
)
# FX and basis are synthetic assumptions, NOT Bloomberg quotes.
# FX = JPY per foreign unit, for exchange on each bond's effective date.
# Basis = JPY leg spread: pay foreign overnight / receive TONAR + basis.
issue_date <- "2026-09-30"

# 4. Same fixed-bond factory and native ASW calculation.
bonds <- tbl_bonds$currency |> purrr::set_names() |>
  purrr::map(function(ccy) {
    row <- tbl_bonds |> filter(currency == ccy)
    GaussQuant::fixed_rate_bond_GQL(
      issue_date = issue_date, maturity_date = row$maturity_date,
      coupon_rate = row$coupon_rate, face_amount = 100, redemption = 100,
      settlement_days = 2L, frequency = QuantLib::Period("Semiannual"),
      calendar = curves[[ccy]]$calendar,
      accrual_day_counter = if (ccy == "JPY") QuantLib::Actual365Fixed()
                            else QuantLib::Thirty360("BondBasis"),
      payment_convention = "Following", schedule_convention = "Unadjusted",
      maturity_convention = "Unadjusted"
    )
  })
native_asw <- tbl_bonds$currency |> purrr::set_names() |>
  purrr::map(function(ccy) {
    row <- tbl_bonds |> filter(currency == ccy)
    GaussQuant::ois_asset_swap_GQL(bonds[[ccy]], row$clean_price, curves[[ccy]])
  })
tbl_native_asw <- purrr::imap_dfr(native_asw, function(x, ccy) {
  x$valuation |> mutate(currency = ccy, .before = 1)
})
print(tbl_native_asw)

# 5. Foreign bonds converted to JPY under the same single-quote CCS model.
foreign_ccys <- setdiff(tbl_bonds$currency, "JPY")
jpy_asw <- foreign_ccys |> purrr::set_names() |>
  purrr::map(function(ccy) {
    row <- tbl_bonds |> filter(currency == ccy)
    GaussQuant::ois_asset_swap_jpy_basis_GQL(
      bond = bonds[[ccy]], clean_price = row$clean_price,
      foreign_curve = curves[[ccy]], jpy_curve = curves$JPY,
      fx_spot = row$fx_spot, basis_bp = row$basis_bp, currency = ccy
    )
  })
tbl_jpy_asw <- purrr::map_dfr(jpy_asw, function(x) x$valuation)
print(tbl_jpy_asw |> select(
  currency, native_asw_bp, basis_on_jpy_bp, jpy_asw_bp,
  fair_spread_npv_jpy, ccs_repricing_npv_jpy
))
stopifnot(
  all(abs(tbl_jpy_asw$fair_spread_npv_jpy) < 1e-6),
  all(abs(tbl_jpy_asw$ccs_repricing_npv_jpy) < 1e-6)
)

# 6. All price sensitivities are generated here, not in another sample file.
tbl_native_prices <- purrr::imap_dfr(native_asw, function(x, ccy) {
  GaussQuant::ois_asset_swap_price_table_GQL(x, seq(95, 105, .5)) |>
    mutate(currency = ccy, .before = 1)
})
tbl_jpy_prices <- purrr::map_dfr(foreign_ccys, function(ccy) {
  row <- tbl_bonds |> filter(currency == ccy)
  purrr::map_dfr(seq(95, 105, .5), function(price) {
    GaussQuant::ois_asset_swap_jpy_basis_GQL(
      bonds[[ccy]], price, curves[[ccy]], curves$JPY,
      row$fx_spot, row$basis_bp, currency = ccy
    )$valuation |> mutate(clean_price = price, .before = 1)
  })
})

# 7. Optional historical TONAR example also lives in this same file.
# It uses a different evaluation date, so run it only AFTER all 2026 calculations.
historical <- NULL
if (run_historical) {
  historical <- local({
    historical_date <- as.Date("2022-08-19")
    cb <- ois_curve_from_quotes(
      "JPY", historical_date, tonar_market_quotes(historical_date)
    )
    bond <- GaussQuant::fixed_rate_bond_GQL(
      issue_date = "2022-07-28", maturity_date = "2025-07-28",
      coupon_rate = .0037, face_amount = 100, redemption = 100, settlement_days = 2L,
      frequency = QuantLib::Period("Semiannual"), calendar = cb$calendar,
      accrual_day_counter = QuantLib::Thirty360("BondBasis"),
      payment_convention = "Following", schedule_convention = "Unadjusted",
      maturity_convention = "Unadjusted"
    )
    x <- GaussQuant::ois_asset_swap_GQL(bond, 97, cb, spread_bp = 130)
    list(curve = cb, bond = bond, asw = x,
         prices = GaussQuant::ois_asset_swap_price_table_GQL(x, seq(96, 98, .05)))
  })
  print(historical$asw$valuation)
}
# Restore global QuantLib evaluation date after the optional historical section.
GaussQuant::set_eval_date_GQL(GaussQuant::date_GQL(as.character(marketdate)))
results <- list(
  marketdate = marketdate, market_quotes = tbl_market_quotes,
  excluded_quotes = tbl_excluded_quotes, curves = curves, bonds = bonds,
  quote_repricing = tbl_quote_repricing,
  native_asw = native_asw, jpy_asw = jpy_asw,
  native_prices = tbl_native_prices, jpy_prices = tbl_jpy_prices,
  historical = historical
)



}

if (run_worksheet_demo) {
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl", repos = "https://cloud.r-project.org")
}



.ws_require(c("writexl", "readxl", "readr", "dplyr", "purrr", "tibble", "collapse"))

dir.create("data/demo", recursive = TRUE, showWarnings = FALSE)
demo_path <- tempfile("worksheet_", tmpdir = "data/demo")
purrr::walk(c("offers/YEN", "offers/ISO", "aladdin", "market"), function(folder) {
  dir.create(file.path(demo_path, folder), recursive = TRUE, showWarnings = FALSE)
})

# 1. Independent YEN and ISO Excel batches.
# Extra stale currency/coupon_rate columns deliberately test replacement by RDS.
yen_offers <- tibble::tibble(
  offer_date = as.Date("2026-10-06"), ISIN = "JP0000000001",
  broker_names = "DEMO_BROKER_A", offer_price = 99.5, offer_size = 100000000,
  offer_settlementdate = as.Date("2026-10-08"),
  currency = "STALE", coupon_rate = 0.99
)
iso_offers <- tibble::tibble(
  offer_date = as.Date(c("2026-10-06", "2026-10-06", "2026-10-06", "2026-10-07")),
  ISIN = c("US0000000001", "US0000000001", "GB0000000001", "US0000000001"),
  broker_names = c("DEMO_BROKER_A", "DEMO_BROKER_B", "DEMO_BROKER_C", "DEMO_BROKER_A"),
  offer_price = c(99.5, 99.7, 100.1, 99.6), offer_size = c(1000000, 2000000, 1000000, 1500000),
  offer_settlementdate = as.Date(c("2026-10-08", "2026-10-08", "2026-10-08", "2026-10-09")),
  currency = "STALE", coupon_rate = 0.99
)
yen_file <- file.path(demo_path, "offers/YEN/demo_yen.xlsx")
iso_file <- file.path(demo_path, "offers/ISO/demo_iso.xlsx")
writexl::write_xlsx(list(Offers = yen_offers), yen_file)
writexl::write_xlsx(list(Offers = iso_offers), iso_file)

# 2. Aladdin-style RDS snapshots (NOT a real Aladdin export).
# GB0000000001 is intentionally absent. A future USD record must not be selected.
bond_master <- tibble::tibble(
  ISIN = c("JP0000000001", "US0000000001", "US0000000001"),
  attribute_date = as.Date(c("2026-10-06", "2026-10-06", "2026-10-08")),
  currency = c("JPY", "USD", "USD"), coupon_type = "FIXED",
  coupon_rate = c(0.03, 0.05, 0.06),
  issue_date = as.Date("2026-09-30"), maturity_date = as.Date("2036-09-30"),
  coupon_frequency = 2L, day_count = c("Actual365Fixed", "Thirty360(BondBasis)", "Thirty360(BondBasis)"),
  redemption = 100
)
master_file <- file.path(demo_path, "aladdin/demo_bond_master.rds")
saveRDS(bond_master, master_file)

# 3. Pipe-delimited OIS quotes. No 7 October market data, deliberately.
# Quote count/curve coverage is outside this input-validation demo.
market_quotes <- tibble::tibble(
  marketdate = as.Date("2026-10-06"), currency = c("JPY", "JPY", "USD", "USD"),
  instrument = c("TONAR_OIS", "TONAR_OIS", "SOFR_OIS", "SOFR_OIS"),
  tenor = c("1Y", "10Y", "1Y", "10Y"), rate_pct = c(1.65, 2.96, 4.455, 4.891)
)
market_file <- file.path(demo_path, "market/demo_ois.txt")
readr::write_delim(market_quotes, market_file, delim = "|")

# 4. One workflow, inclusive date range. All five offers must remain.
offer_batches <- list(list(path = yen_file, sheet = "Offers"), list(path = iso_file, sheet = "Offers"))
worksheet <- workflows_validate_worksheet(
  from = as.Date("2026-10-06"), to = as.Date("2026-10-07"),
  offer_path = offer_batches, master_path = master_file, market_path = market_file,
  attribute_policy = "exact"
)
print(worksheet |> dplyr::select(
  offer_date, ISIN, broker_names, offer_price, offer_size, offer_settlementdate,
  currency, coupon_rate, attribute_date, marketdate, validation_status, validation_message
), n = Inf, width = Inf)

tbl_validation_summary <- worksheet |> dplyr::count(offer_date, validation_status)
print(tbl_validation_summary)
tbl_valid_offers <- worksheet |> dplyr::filter(validation_status == "valid")
tbl_invalid_offers <- worksheet |> dplyr::filter(validation_status == "invalid")

# Expected: 3 valid on 6 October (JPY + two USD brokers), 2 invalid.
# GB: missing master. 7 October USD: missing same-day master and market.
stopifnot(
  nrow(worksheet) == 5L, nrow(tbl_valid_offers) == 3L, nrow(tbl_invalid_offers) == 2L,
  sum(tbl_valid_offers$ISIN == "US0000000001") == 2L,
  all(tbl_valid_offers$currency %in% c("JPY", "USD")),
  all(tbl_valid_offers$coupon_rate %in% c(0.03, 0.05)),
  all(worksheet$attribute_date <= worksheet$offer_date, na.rm = TRUE),
  any(grepl("missing_bond_attributes", worksheet$validation_message)),
  any(grepl("missing_currency_market_data", worksheet$validation_message))
)

# 5. Explicit historical attribute selection: 7 October can use 6 October RDS.
# Missing 7 October market data must STILL leave that offer invalid.
worksheet_previous <- workflows_validate_worksheet(
  from = as.Date("2026-10-06"), to = as.Date("2026-10-07"),
  offer_path = offer_batches, master_path = master_file, market_path = market_file,
  attribute_policy = "previous"
)
previous_day7 <- worksheet_previous |>
  dplyr::filter(offer_date == as.Date("2026-10-07"))
stopifnot(
  previous_day7$attribute_date == as.Date("2026-10-06"),
  previous_day7$attribute_age_days == 1L, previous_day7$coupon_rate == 0.05,
  previous_day7$validation_status == "invalid"
)

# 6. Empty period must return a tibble with zero rows, not an error.
worksheet_empty <- workflows_validate_worksheet(
  from = as.Date("2026-10-09"), to = as.Date("2026-10-09"),
  offer_path = offer_batches, master_path = master_file, market_path = market_file
)
stopifnot(inherits(worksheet_empty, "tbl_df"), nrow(worksheet_empty) == 0L)

# Synthetic inputs stay available for inspection. No actual source data is used.
results_worksheet_demo <- list(
  path = normalizePath(demo_path, winslash = "/"),
  worksheet = worksheet, worksheet_previous = worksheet_previous,
  summary = tbl_validation_summary
)
message("Demo checks passed. Synthetic input files: ", results_worksheet_demo$path)

}

if (run_ccs_checks) local({
# Deterministic fixtures test PV identities independently of QuantLib curves.
start <- as.Date("2026-10-14")
dates <- start + 365 * 1:10
N <- 100
make_curve <- function(rate) list(
  evaluation_date = as.character(start),
  curve = list(discount = function(d) {
    exp(-rate * as.numeric(as.Date(QuantLib::Date_ISO(d)) - start) / 365)
  })
)
fc <- make_curve(0.04)
jc <- make_curve(0.01)
make_template <- function(r, clean = 99.5, coupon = 5) {
  df <- exp(-r * (1:10))
  float <- rep(N * expm1(r), 10)
  bond <- rep(coupon, 10)
  bond[10] <- bond[10] + N
  upfront <- N * (clean / 100 - 1)
  ann <- sum(N * df)
  fair <- (sum(bond * df) - sum(float * df) - N * df[10] - upfront) / ann
  list(
    effective_date = start, maturity_date = tail(dates, 1),
    valuation = data.frame(
      spread = 0, nominal = N, spread_annuity = ann,
      fair_spread_bp = fair * 1e4, upfront_pv = upfront
    ),
    cashflows = data.frame(
      leg = c(rep("pay_bond", 10), rep("receive_overnight", 10),
              "receive_principal", "upfront"),
      payment_date = c(dates, dates, tail(dates, 1), start),
      amount = c(-bond, float, N, upfront)
    )
  )
}
calc <- function(basis = 0, fx = 150, price = 99.5, coupon = 5) {
  GaussQuant::ois_asset_swap_jpy_from_legs_GQL(
    make_template(.04, price, coupon), make_template(.01, price, coupon),
    fc, jc, fx, basis, "USD"
  )
}
zero <- calc()
negative <- calc(-30)
positive <- calc(30)
stopifnot(
  abs(zero$valuation$calibrated_discount_spread_bp) < 1e-7,
  negative$valuation$jpy_asw_bp < zero$valuation$jpy_asw_bp,
  positive$valuation$jpy_asw_bp > zero$valuation$jpy_asw_bp,
  abs(calc(-30, 100)$valuation$jpy_asw_bp -
      calc(-30, 200)$valuation$jpy_asw_bp) < 1e-7,
  calc(price = 100.5)$valuation$jpy_asw_bp < calc(price = 99.5)$valuation$jpy_asw_bp,
  abs(calc(coupon = 100 * expm1(.04), price = 100)$valuation$jpy_asw_bp) < 1e-7,
  abs(negative$fx_forward(start) - 150) < 1e-10
)
purrr::walk(c(-50, -30, 0, 30, 50), function(b) {
  x <- calc(b)$valuation
  stopifnot(abs(x$ccs_repricing_npv_jpy) < 1e-6,
            abs(x$fair_spread_npv_jpy) < 1e-6)
})
print(rbind(negative$valuation, zero$valuation, positive$valuation))
cat("PASS: zero basis, basis direction, FX scale invariance, price sensitivity, par floater, CCS and ASW repricing.\n")




})
