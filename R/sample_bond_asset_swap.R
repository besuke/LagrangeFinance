# Unified sample: run this ONE file from the LagrangeFinance project root.
# Market observations: 2026-10-06. Bonds / FX / basis below are test assumptions.
library(GaussQuant)
library(dplyr)
library(tibble)
library(purrr)

purrr::walk(c(
  "import_ois_curves.R", "instance_swap_curve.R",
  "set_market_data_for_assetswap.R", "bond_asset_swap.R",
  "cross_currency_asset_swap.R"
), function(f) source(file.path("R", f)))

marketdate <- as.Date("2026-10-06")
path <- "data/raw/market/Bloomberg_MarketData_20261006.xlsx"

# 1. Import; all quotes use the same marketdate/instrument/tenor/rate schema.
# JPY remains an explicitly identified legacy source until an Excel JPY tab exists.
excel_sheets <- readxl::excel_sheets(path)
jpy_quotes <- if ("JPY" %in% excel_sheets) {
  import_ois_curves(path, "JPY", marketdate)
} else {
  tonar_market_quotes(marketdate)
}
tbl_market_quotes <- purrr::map_dfr(
  c("USD", "EUR", "GBP", "AUD"),
  function(ccy) import_ois_curves(path, ccy, marketdate)
) |> bind_rows(jpy_quotes)

# 2. Do not treat a historical fixing or an unconfirmed AUD IRS as spot OIS.
tbl_excluded_quotes <- tbl_market_quotes |>
  filter(!instrument_type %in% c("spot_ois", "deposit"))
tbl_curve_quotes <- tbl_market_quotes |>
  filter(instrument_type %in% c("spot_ois", "deposit"))
if (nrow(tbl_excluded_quotes)) print(tbl_excluded_quotes, n = Inf)

# 3. Same curve builder, same date and same return fields for every currency.
currencies <- c("JPY", "USD", "EUR", "GBP", "AUD")
curves <- currencies |> purrr::set_names() |>
  purrr::map(function(ccy) instance_swap_curve(
    currency = ccy, marketdate = marketdate, quotes = tbl_curve_quotes
  ))
tbl_quote_repricing <- purrr::map_dfr(curves, function(x) x$quote_repricing)
tbl_curve_summary <- purrr::map_dfr(curves, function(x) tibble(
  currency = x$currency, index = x$index_name, marketdate = x$marketdate,
  quotes = nrow(x$quotes), last_tenordate = max(x$quotes$tenordate),
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
    compute_asset_swap(bonds[[ccy]], row$clean_price, curves[[ccy]])
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
    compute_asset_swap_jpy(
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
  asset_swap_price_table(x, seq(95, 105, .5)) |>
    mutate(currency = ccy, .before = 1)
})
tbl_jpy_prices <- purrr::map_dfr(foreign_ccys, function(ccy) {
  row <- tbl_bonds |> filter(currency == ccy)
  purrr::map_dfr(seq(95, 105, .5), function(price) {
    compute_asset_swap_jpy(
      bonds[[ccy]], price, curves[[ccy]], curves$JPY,
      row$fx_spot, row$basis_bp, currency = ccy
    )$valuation |> mutate(clean_price = price, .before = 1)
  })
})

# 7. Optional historical TONAR example also lives in this same file.
# It uses a different evaluation date, so run it only AFTER all 2026 calculations.
run_historical <- FALSE
historical <- NULL
if (run_historical) {
  historical <- local({
    historical_date <- as.Date("2022-08-19")
    cb <- instance_swap_curve(
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
    x <- compute_asset_swap(bond, 97, cb, spread_bp = 130)
    list(curve = cb, bond = bond, asw = x,
         prices = asset_swap_price_table(x, seq(96, 98, .05)))
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

