# Part III ch04-ch07: remove third-level headings and normalize chunk labels
# Run from: C:/AnalyticFin/Projects/LagrangeFinance
# This script changes only Markdown ### headings and Quarto chunk labels.
# Body text, equations, R code, ## section headings, and YAML titles are preserved.

root <- normalizePath(".", winslash = "/", mustWork = TRUE)

files <- c(
  ch04 = file.path(root, "part3_enterprise_riemann", "ch04_automatic_differentiation.qmd"),
  ch05 = file.path(root, "part3_enterprise_riemann", "ch05_option_greeks_autodiff.qmd"),
  ch06 = file.path(root, "part3_enterprise_riemann", "ch06_interest_rate_risk_autodiff.qmd"),
  ch07 = file.path(root, "part3_enterprise_riemann", "ch07_prdc_autodiff.qmd")
)

stopifnot(all(file.exists(files)))

label_maps <- list(
  ch04 = c(
    "autodiff-01" = "autodiff-single-variable-graph",
    "autodiff-02" = "autodiff-single-variable-gradient",
    "autodiff-03" = "autodiff-analytic-gradient",
    "autodiff-04" = "autodiff-multivariable-gradient"
  ),
  ch05 = c(
    "option-input" = "option-market-input",
    "gaussquant-option" = "option-gaussquant-pricing",
    "gaussquant-option-summary" = "option-gaussquant-greeks",
    "monte-carlo-random-numbers" = "option-mc-common-random-numbers",
    "monte-carlo-pricing-function" = "option-mc-pricing-function",
    "option-risk-tensors" = "option-mc-risk-tensors",
    "monte-carlo-option-npv" = "option-mc-present-value",
    "monte-carlo-first-order-greeks" = "option-mc-first-order-greeks",
    "smooth-payoff-function" = "option-smooth-payoff-function",
    "smooth-monte-carlo-pricing-function" = "option-smooth-mc-pricing-function",
    "monte-carlo-gamma-graph" = "option-gamma-computational-graph",
    "monte-carlo-delta-for-gamma" = "option-delta-for-gamma",
    "monte-carlo-gamma" = "option-mc-gamma",
    "monte-carlo-greeks-summary" = "option-mc-greeks-summary",
    "gaussquant-monte-carlo-comparison" = "option-gaussquant-mc-comparison",
    "monte-carlo-standard-error" = "option-mc-standard-error"
  ),
  ch06 = c(
    "interest-rate-risk-01" = "swap-market-input",
    "interest-rate-risk-02" = "swap-payment-schedule",
    "interest-rate-risk-03" = "swap-risk-tensors",
    "interest-rate-risk-04" = "swap-discount-factors",
    "interest-rate-risk-05" = "swap-fixed-leg-present-value",
    "interest-rate-risk-06" = "swap-floating-leg-present-value",
    "interest-rate-risk-07" = "swap-present-value",
    "interest-rate-risk-08" = "swap-autodiff-rate-gradient",
    "interest-rate-risk-09" = "swap-autodiff-dv01-summary",
    "interest-rate-risk-10" = "swap-flat-rate-pricing-function",
    "interest-rate-risk-11" = "swap-finite-difference-up-bump",
    "interest-rate-risk-12" = "swap-finite-difference-down-bump",
    "interest-rate-risk-13" = "yield-curve-node-data",
    "interest-rate-risk-14" = "yield-curve-risk-tensors",
    "interest-rate-risk-15" = "yield-curve-cash-flows",
    "interest-rate-risk-16" = "yield-curve-present-value",
    "interest-rate-risk-17" = "yield-curve-autodiff-gradient",
    "interest-rate-risk-18" = "yield-curve-bucketed-dv01",
    "interest-rate-risk-19" = "yield-curve-gps",
    "interest-rate-risk-20" = "yield-curve-total-dv01"
  ),
  ch07 = c(
    "prdc-autodiff-01" = "prdc-market-input",
    "prdc-autodiff-02" = "prdc-jpy-foreign-curves",
    "prdc-autodiff-03" = "prdc-mc-common-random-numbers",
    "prdc-autodiff-04" = "prdc-market-risk-tensors",
    "prdc-autodiff-05" = "prdc-contract-tensors",
    "prdc-autodiff-06" = "prdc-mc-pricing-function",
    "prdc-autodiff-07" = "prdc-mc-present-value",
    "prdc-autodiff-08" = "prdc-autodiff-risk-gradient",
    "prdc-autodiff-09" = "prdc-fx-delta",
    "prdc-autodiff-10" = "prdc-jpy-curve-dv01",
    "prdc-autodiff-11" = "prdc-foreign-curve-dv01",
    "prdc-autodiff-12" = "prdc-fx-vega",
    "prdc-autodiff-13" = "prdc-fx-vega01",
    "prdc-autodiff-14" = "prdc-risk-summary",
    "prdc-autodiff-15" = "prdc-curve-risk-summary",
    "prdc-autodiff-16" = "prdc-finite-difference-fx-delta"
  )
)

read_qmd <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

write_qmd <- function(lines, path) {
  writeLines(lines, path, useBytes = TRUE)
}

remove_h3 <- function(lines) {
  lines[!grepl("^###\\s+", lines)]
}

replace_labels <- function(lines, map) {
  for (old in names(map)) {
    old_line <- paste0("#| label: ", old)
    new_line <- paste0("#| label: ", unname(map[[old]]))
    lines[lines == old_line] <- new_line
  }
  lines
}

audit_file <- function(path, expected_title, expected_h2, forbidden_labels = character()) {
  x <- read_qmd(path)
  h3 <- grep("^###\\s+", x, value = TRUE)
  h2 <- grep("^##\\s+", x, value = TRUE)
  labels <- sub("^#\\|\\s*label:\\s*", "", grep("^#\\|\\s*label:", x, value = TRUE))

  stopifnot(length(h3) == 0L)
  stopifnot(any(x == paste0("title: \"", expected_title, "\"")))
  stopifnot(identical(h2, expected_h2))
  stopifnot(!any(duplicated(labels)))
  stopifnot(!any(labels %in% forbidden_labels))
}

expected <- list(
  ch04 = c(
    "## III-4.1 金融計算と微分",
    "## III-4.2 有限差分法",
    "## III-4.3 自動微分の原理と実装",
    "## III-4.4 深層学習ライブラリによる市場リスク感応度"
  ),
  ch05 = c(
    "## III-5.1 オプション評価とリスク感応度",
    "## III-5.2 ブラック・ショールズ式による解析解",
    "## III-5.3 モンテカルロ法における自動微分のリスク感応度",
    "## III-5.4 微分不可なオプションペイオフのケーススタディ"
  ),
  ch06 = c(
    "## III-6.1 金利スワップ評価とリスク感応度",
    "## III-6.2 金利リスクにおける自動微分",
    "## III-6.3 イールドカーブと自動微分",
    "## III-6.4 バケットDV01とGPS"
  ),
  ch07 = c(
    "## III-7.1 金利為替系デリバティブと多因子市場モデル",
    "## III-7.2 多因子モンテカルロ法",
    "## III-7.3 多因子リスク感応度と自動微分",
    "## III-7.4 非線形ペイオフと自動微分",
    "## III-7.5 エンタープライズ・リスク計算"
  )
)

titles <- c(
  ch04 = "第III-4章 自動微分計算",
  ch05 = "第III-5章 自動微分応用：オプション",
  ch06 = "第III-6章 自動微分応用：金利リスク",
  ch07 = "第III-7章 自動微分応用：多因子モデル"
)

# Backups are intentionally not created because the current clean state is committed in Git.
for (key in names(files)) {
  x <- read_qmd(files[[key]])
  x <- remove_h3(x)
  x <- replace_labels(x, label_maps[[key]])
  write_qmd(x, files[[key]])
}

for (key in names(files)) {
  audit_file(
    files[[key]],
    expected_title = titles[[key]],
    expected_h2 = expected[[key]],
    forbidden_labels = names(label_maps[[key]])
  )
}

cat("\nPart III ch04-ch07 updated.\n\n")
for (key in names(files)) {
  x <- read_qmd(files[[key]])
  cat(basename(files[[key]]), "\n", sep = "")
  cat(paste(grep("^##\\s+", x, value = TRUE), collapse = "\n"), "\n\n")
}
cat("AUDIT OK\n")
