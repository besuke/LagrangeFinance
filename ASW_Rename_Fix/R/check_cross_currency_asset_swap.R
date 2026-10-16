source("R/compute_asset_swap_basis_JPY.R")
# Deterministic fixtures test PV identities independently of GaussQuant.
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
  asset_swap_jpy_from_legs(
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
invisible(Map(function(b) {
  x <- calc(b)$valuation
  stopifnot(abs(x$ccs_repricing_npv_jpy) < 1e-6,
            abs(x$fair_spread_npv_jpy) < 1e-6)
}, c(-50, -30, 0, 30, 50)))
print(rbind(negative$valuation, zero$valuation, positive$valuation))
cat("PASS: zero basis, basis direction, FX scale invariance, price sensitivity, par floater, CCS and ASW repricing.\n")



