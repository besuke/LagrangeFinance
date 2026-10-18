# Run this ONE sample from the LagrangeFinance project root.
# ALL records, identifiers, rates and broker names below are SYNTHETIC.
# Creates a new demo directory each run; never overwrites real market/offer data.
# Requires the six accompanying worksheet function files already in R/.

if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl", repos = "https://cloud.r-project.org")
}

purrr::walk(c(
  "worksheet_helpers.R", "import_offer_sheet.R", "import_bond_master_attribute_ADP.R",
  "import_market_data.R", "merge_offer_bond_attributes.R", "workflows_validate_worksheet.R"
), function(file) source(file.path("R", file)))

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
