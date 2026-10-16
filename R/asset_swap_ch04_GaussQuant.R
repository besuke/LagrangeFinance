library(GaussQuant)

set_eval_date_GQL(date_GQL("2022-08-19"))
cal <- QuantLib::Japan()
dc <- QuantLib::Actual365Fixed()
dc_bond <- QuantLib::Thirty360("BondBasis")

# TONA quotes (%); deposit via Tonar, OIS spot lag 2 days.
rates_pct <- c(
  -0.009, -0.01807, -0.01043, 0.0125, 0.03125,
  0.04875, 0.07375, 0.11854, 0.19146
)
tenors <- c("1m", "6m", "12m", "18m", "2y", "3y", "5y", "7y")
index0 <- tonar_GQL()
helpers <- list(QuantLib::DepositRateHelper(quote_handle_GQL(rates_pct[1] / 100), index0))
for (i in seq_along(tenors)) {
  helpers[[i + 1L]] <- QuantLib::OISRateHelper(
    2L, QuantLib::Period(
      as.integer(sub("[my]$", "", tenors[i])),
      if (grepl("m$", tenors[i])) "Months" else "Years"
    ),
    quote_handle_GQL(rates_pct[i + 1L] / 100), index0
  )
}
curve <- QuantLib::PiecewiseLogCubicDiscount(
  0L, cal, push_rate_helpers_GQL(helpers), dc
)

handle <- QuantLib::YieldTermStructureHandle(curve)
tona <- tonar_GQL(handle)

bond <- fixed_rate_bond_GQL(
  issue_date = "2022-07-28", maturity_date = "2025-07-28",
  coupon_rate = 0.0037, face_amount = 100, settlement_days = 2L,
  frequency = QuantLib::Period("Semiannual"), calendar = cal,
  accrual_day_counter = dc_bond, payment_convention = "Following",
  schedule_convention = "Unadjusted", maturity_convention = "Unadjusted"
)
invisible(set_bond_pricing_engine_GQL(bond, curve))


floating <- QuantLib::Schedule(
  date_GQL("2022-08-23"),
  date_GQL("2025-07-28"),
  QuantLib::Period("Annual"),
  cal,
  "Unadjusted",
  "Unadjusted",
  QuantLib::copyToR(QuantLib::DateGeneration(), "Backward"),
  FALSE
)

tbl_bond_cashflow <- GaussQuant::bond_cashflow_table_GQL(
  bond = bond
)

GaussQuant::show_tbl_GQH(
  tbl_bond_cashflow,
  "債券キャッシュフロー",
  n = Inf
)
tbl_bond_cashflow <- GaussQuant::bond_cashflow_table_GQL(
  bond = bond
) |>
  dplyr::mutate(
    discount_factor = purrr::map_dbl(
      as.character(date),
      function(pay_date) {
        QuantLib::YieldTermStructure_discount(
          handle,
          date_GQL(pay_date)
        )
      }
    ),
    present_value = amount * discount_factor
  )

GaussQuant::show_tbl_GQH(
  tbl_bond_cashflow,
  "債券キャッシュフローとTONAカーブによる割引評価",
  n = Inf
)

tbl_bond_cashflow <- GaussQuant::bond_cashflow_table_GQL(
  bond = bond
) |>
  dplyr::mutate(
    discount_factor = purrr::map_dbl(
      as.character(date),
      function(pay_date) {
        QuantLib::YieldTermStructure_discount(
          curve,
          date_GQL(pay_date)
        )
      }
    ),
    present_value = amount * discount_factor
  )

GaussQuant::show_tbl_GQH(
  tbl_bond_cashflow,
  "債券キャッシュフローとTONAカーブによる割引評価",
  n = Inf
)
library(styler)
asw <- make_asset_swap_GQL(
  bond = bond, clean_price = 97, floating_schedule = floating,
  index = tona, discount_handle = handle, floating_day_counter = dc,
  maturity_date = "2025-07-28", pay_bond_coupon = TRUE,
  spread = 0.013, par_asset_swap = TRUE
)

tbl_res_asset <- tibble::tibble(
  AS_spread_pct = 100 * asset_swap_fair_spread_GQL(asw),
  fair_clean_price = asset_swap_fair_clean_price_GQL(asw)
)
tbl_res_asset


tbl_asset_swap <- purrr::map_dfr(
  seq(96, 98, by = 0.05),
  function(price) {
    asw <- make_asset_swap_GQL(
      bond = bond,
      clean_price = price,
      floating_schedule = floating,
      index = tona,
      discount_handle = handle,
      floating_day_counter = dc,
      maturity_date = "2025-07-28",
      pay_bond_coupon = TRUE,
      spread = 0.013,
      par_asset_swap = TRUE
    )

    tibble::tibble(
      clean_price = price,
      AS_spread_pct = 10000 * asset_swap_fair_spread_GQL(asw) / 100
    )
  }
)

tbl_asset_swap |> print(n = Inf)
source("jscc_tonar_quotes_GQL.R")
source("tonar_asset_swap_GQL.R")

cb <- jscc_tonar_curve_GQL()

asw <- make_tonar_asset_swap_GQL(
  bond = bond,
  clean_price = 99.5,
  forecast_handle = cb$curve_handle,
  discount_curve = cb$curve,
  effective_date = "2026-10-14",
  spread = 0.001
)

tonar_asset_swap_summary_GQL(asw)
asw$cashflows