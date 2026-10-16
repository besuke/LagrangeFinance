source("R/bond_asset_swap.R")
source("R/set_market_data_for_assetswap.R")
source("R/import_ois_curves.R")
library(GaussQuant)
library(dplyr)
library(tibble)


path <- "data/raw/market/Bloomberg_MarketData_20261006.xlsx"

usd_quotes <- import_ois_curves(path, "USD")
eur_quotes <- import_ois_curves(path, "EUR")
gbp_quotes <- import_ois_curves(path, "GBP")
aud_quotes <- import_ois_curves(path, "AUD")

usd_quotes

tonar <- instance_swap_curve_TONAR("2022-08-19")

bond <- GaussQuant::fixed_rate_bond_GQL(
  issue_date = "2022-07-28",
  maturity_date = "2025-07-28",
  coupon_rate = 0.0037,
  face_amount = 100,
  redemption = 100,
  settlement_days = 2L,
  frequency = QuantLib::Period("Semiannual"),
  calendar = QuantLib::Japan(),
  accrual_day_counter = QuantLib::Thirty360("BondBasis"),
  payment_convention = "Following",
  schedule_convention = "Unadjusted",
  maturity_convention = "Unadjusted"
)
tbl_bond_cashflow <- bond_cashflow_table_GQL(bond) |>
  dplyr::mutate(
    discount_factor = purrr::map_dbl(
      as.character(date),
      function(d) tonar$curve$discount(date_GQL(d))
    ),
    present_value = amount * discount_factor
  )
print(tbl_bond_cashflow, n = Inf)
asw <- compute_asset_swap(
  bond = bond,
  clean_price = 97,
  swap_curve = tonar,
  spread_bp = 130
)
fair_clean_price <- asset_swap_fair_price(asw)

tbl_res_asset <- tibble::tibble(
  clean_price = 97,
  AS_spread_bp = asw$valuation$fair_spread_bp,
  AS_spread_pct = asw$valuation$fair_spread * 100,
  fair_clean_price = fair_clean_price
)
print(tbl_res_asset)
tbl_asset_swap <- asset_swap_price_table(asw, seq(96, 98, by = 0.05))
print(tbl_asset_swap, n = Inf)

result_2022 <- list(
  valuation = asw$valuation, cashflows = asw$cashflows,
  prices = tbl_asset_swap
)

curve <- instance_swap_curve_TONAR("2026-10-09")

bond <- fixed_rate_bond_GQL(
  issue_date = "2026-10-14",
  maturity_date = "2046-10-14",
  coupon_rate = 0.036,
  face_amount = 100,
  redemption = 100,
  settlement_days = 2L,
  frequency = QuantLib::Period("Semiannual"),
  calendar = QuantLib::Japan(),
  accrual_day_counter = QuantLib::Actual365Fixed(),
  payment_convention = "Following",
  schedule_convention = "Unadjusted",
  maturity_convention = "Unadjusted"
)

tbl_bond_cashflow <- bond_cashflow_table_GQL(bond) |>
  dplyr::mutate(
    discount_factor = purrr::map_dbl(
      as.character(date),
      function(d) curve$curve$discount(date_GQL(d))
    ),
    present_value = amount * discount_factor
  )
print(tbl_bond_cashflow, n = Inf)
asw <- compute_asset_swap(
  bond, 99.5, curve
)
print(asw$valuation)

tbl_asset_swap <- asset_swap_price_table(asw, seq(95, 105, by = 0.50))
print(tbl_asset_swap, n = Inf)
result_2026 <- list(
  valuation = asw$valuation, cashflows = asw$cashflows,
  prices = tbl_asset_swap
)

library(GaussQuant)
library(dplyr)
library(tibble)

source("R/bond_asset_swap.R")
source("R/set_market_data_for_assetswap.R")
source("R/instance_swap_curve.R")

# 既存TONARデータがある日付に合わせる
evaluation_date <- as.Date("2026-10-09")

# 年限ごとの仮の「SOFR − TONAR」差：単位は％ポイント
# 自然3次スプラインで中間年限を補間
spread_spline <- stats::splinefun(
  x = c(1 / 12, 1, 5, 10, 20, 30),
  y = c(2.50,   2.30, 1.80, 1.50, 1.20, 1.00),
  method = "natural"
)

usd_ois_quotes <- Glumgamma |>
  filter(date == evaluation_date) |>
  mutate(
    tenor_years = as.numeric(sub("[MY]$", "", tenor)) /
      if_else(grepl("M$", tenor), 12, 1),
    tonar_rate_pct = rate_pct,
    added_spread_pct = spread_spline(tenor_years),
    rate_pct = tonar_rate_pct + added_spread_pct,
    rate = rate_pct / 100,
    currency = "USD"
  ) |>
  select(
    date, currency, tenor, rate_pct, rate,
    tonar_rate_pct, added_spread_pct
  )

print(usd_ois_quotes, n = Inf)

# テスト用SOFRカーブ
usd_curve <- instance_swap_curve(
  currency = "USD",
  date = evaluation_date,
  quotes = usd_ois_quotes
)

# 仮のドル建て固定利付債：10年・年率5％・半年払い
usd_bond <- GaussQuant::fixed_rate_bond_GQL(
  issue_date = "2026-10-14",
  maturity_date = "2036-10-14",
  coupon_rate = 0.05,
  face_amount = 100,
  redemption = 100,
  settlement_days = 2L,
  frequency = QuantLib::Period("Semiannual"),
  calendar = usd_curve$calendar,
  accrual_day_counter = QuantLib::Thirty360("BondBasis"),
  payment_convention = "Following",
  schedule_convention = "Unadjusted",
  maturity_convention = "Unadjusted"
)

# SOFRに対するアセットスワップ・スプレッド
asw <- compute_asset_swap(
  bond = usd_bond,
  clean_price = 99.5,
  swap_curve = usd_curve,
  payment_frequency = "Semiannual"
)

print(asw$valuation)

# 債券価格ごとのASスプレッド
asset_swap_price_table(
  asset_swap = asw,
  prices = seq(95, 105, by = 0.5)
) |>
  print(n = Inf)
