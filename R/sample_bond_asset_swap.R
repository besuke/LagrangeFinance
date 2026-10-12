tonar <- instance_swap_curve_TONAR("2022-08-19")
bond <- fixed_rate_bond_GQL(
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
invisible(set_bond_pricing_engine_GQL(bond, tonar$curve))
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
  spread_bp = 130,
  payment_frequency = "Annual"
)
fair_clean_price <- uniroot(function(price) {
  compute_asset_swap(
    bond, price, tonar, spread_bp = 130,
    payment_frequency = "Annual"
  )$valuation$npv
}, interval = c(1, 200), tol = 1e-10)$root

tbl_res_asset <- tibble::tibble(
  clean_price = 97,
  AS_spread_bp = asw$valuation$fair_spread_bp,
  AS_spread_pct = asw$valuation$fair_spread * 100,
  fair_clean_price = fair_clean_price
)
print(tbl_res_asset)
 tbl_asset_swap <- purrr::map_dfr(seq(96, 98, by = 0.05), function(price) {
  result <- compute_asset_swap(
    bond, price, tonar, payment_frequency = "Annual"
  )
  tibble::tibble(
    clean_price = price,
    AS_spread_bp = result$valuation$fair_spread_bp,
    AS_spread_pct = result$valuation$fair_spread * 100
  )
})
print(tbl_asset_swap, n = Inf)

result_2022 <- list(valuation = asw$valuation, cashflows = asw$cashflows,
                    prices = tbl_asset_swap)

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
  bond, 99.5, curve, payment_frequency = "Annual"
)
print(asw$valuation)

tbl_asset_swap <- purrr::map_dfr(seq(95, 105, by = 0.50), function(price) {
  a <- compute_asset_swap(bond, price, curve, payment_frequency = "Annual")
  tibble::tibble(
    clean_price = price,
    AS_spread_bp = a$valuation$fair_spread_bp,
    AS_spread_pct = a$valuation$fair_spread * 100
  )
})
print(tbl_asset_swap, n = Inf)
result_2026 <- list(valuation = asw$valuation, cashflows = asw$cashflows,
                    prices = tbl_asset_swap)
