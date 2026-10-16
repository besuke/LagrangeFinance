# Run from the LagrangeFinance project root.
library(GaussQuant)
library(dplyr)
library(tibble)
source("R/bond_asset_swap.R")
source("R/set_market_data_for_assetswap.R")
source("R/instance_swap_curve.R")
source("R/cross_currency_asset_swap.R")

evaluation_date <- as.Date("2026-10-09")
spread_spline <- stats::splinefun(
  c(1/12, 1, 5, 10, 20, 30),
  c(2.50, 2.30, 1.80, 1.50, 1.20, 1.00), method = "natural"
)
usd_ois_quotes <- Glumgamma |>
  filter(date == evaluation_date) |>
  mutate(
    tenor_years = as.numeric(sub("[MY]$", "", tenor)) /
      if_else(grepl("M$", tenor), 12, 1),
    rate_pct = rate_pct + spread_spline(tenor_years),
    rate = rate_pct / 100, currency = "USD"
  )
usd_curve <- instance_swap_curve("USD", evaluation_date, usd_ois_quotes)
jpy_curve <- instance_swap_curve("JPY", evaluation_date)
usd_bond <- GaussQuant::fixed_rate_bond_GQL(
  issue_date = "2026-10-14", maturity_date = "2036-10-14",
  coupon_rate = .05, face_amount = 100, redemption = 100,
  settlement_days = 2L, frequency = QuantLib::Period("Semiannual"),
  calendar = usd_curve$calendar,
  accrual_day_counter = QuantLib::Thirty360("BondBasis"),
  payment_convention = "Following", schedule_convention = "Unadjusted",
  maturity_convention = "Unadjusted"
)
# Synthetic test assumptions, NOT observed market quotes.
# Exchange at bond settlement: USD 1 = JPY 150.
# CCS: pay SOFR flat, receive TONAR - 30bp.
xasw <- compute_asset_swap_jpy(
  bond = usd_bond, clean_price = 99.5,
  foreign_curve = usd_curve, jpy_curve = jpy_curve,
  fx_spot = 150, basis_bp = -30, currency = "USD"
)
print(xasw$valuation)
print(xasw$hedged_bond_cashflows, row.names = FALSE)

# NPV must be zero at the reported fair JPY spread.
stopifnot(abs(xasw$valuation$fair_spread_npv_jpy) < 1e-6)
stopifnot(abs(xasw$valuation$ccs_repricing_npv_jpy) < 1e-6)

# Price comparison: currency data and CCS conventions remain the same.
tbl_jpy_asw <- purrr::map_dfr(seq(95, 105, .5), function(price) {
  result <- compute_asset_swap_jpy(
    usd_bond, price, usd_curve, jpy_curve, 150, -30, "USD"
  )
  result$valuation |> mutate(clean_price = price, .before = 1)
})
print(tbl_jpy_asw)

