# LagrangeFinance heading audit fix
# This script DOES NOT rewrite any QMD files.
# It only checks the headings already rewritten by rewrite_section_headings_LFL_v2.R.

tbl_chapters <- tibble::tribble(
  ~file, ~part, ~chapter,
  "part1_empirical/ch01_r_basics_tidyverse.qmd",                        "I",   1L,
  "part1_empirical/ch02_financial_data.qmd",                            "I",   2L,
  "part1_empirical/ch03_stock_data_time_series.qmd",                    "I",   3L,
  "part1_empirical/ch04_factor_models.qmd",                             "I",   4L,
  "part1_empirical/ch05_asset_pricing.qmd",                             "I",   5L,
  "part1_empirical/ch06_event_study.qmd",                               "I",   6L,
  "part1_empirical/ch07_machine_learning_factor_selection.qmd",         "I",   7L,
  "part1_empirical/ch08_constrained_portfolio_optimization.qmd",        "I",   8L,
  "part2_quantive_riemann/ch01_deterministic_cashflow_valuation.qmd",   "II",  1L,
  "part2_quantive_riemann/ch02_interest_rate_curves.qmd",               "II",  2L,
  "part2_quantive_riemann/ch03_ois_swaps_multicurve.qmd",               "II",  3L,
  "part2_quantive_riemann/ch04_black_model.qmd",                        "II",  4L,
  "part2_quantive_riemann/ch05_normal_model.qmd",                       "II",  5L,
  "part2_quantive_riemann/ch06_sabr_model.qmd",                         "II",  6L,
  "part2_quantive_riemann/ch07_callable_hull_white.qmd",                "II",  7L,
  "part2_quantive_riemann/ch08_credit_risk_cds.qmd",                    "II",  8L,
  "part2_quantive_riemann/ch09_exotic_derivatives_prdc.qmd",            "II",  9L,
  "part3_enterprise_riemann/ch01_computational_evolution_parallel.qmd", "III", 1L,
  "part3_enterprise_riemann/ch02_distributed_computing_jobtask.qmd",    "III", 2L,
  "part3_enterprise_riemann/ch03_enterprise_financial_computing.qmd",   "III", 3L,
  "part3_enterprise_riemann/ch04_automatic_differentiation.qmd",        "III", 4L,
  "part3_enterprise_riemann/ch05_option_greeks_autodiff.qmd",           "III", 5L,
  "part3_enterprise_riemann/ch06_interest_rate_risk_autodiff.qmd",      "III", 6L,
  "part3_enterprise_riemann/ch07_prdc_autodiff.qmd",                    "III", 7L
)

audit_one_file_LFL <- function(file, part, chapter) {
  lines <- readr::read_lines(file)
  in_fence <- FALSE
  heading_lines <- character()

  purrr::walk(
    lines,
    \(line) {
      if (stringr::str_detect(line, "^\\s*```")) {
        in_fence <<- !in_fence
      } else if (
        !in_fence &&
        stringr::str_detect(line, "^#{2,3}\\s+")
      ) {
        heading_lines <<- base::c(heading_lines, line)
      }
    }
  )

  prefix_h2 <- base::paste0("## ", part, "-", chapter, ".")
  prefix_h3 <- base::paste0("### ", part, "-", chapter, ".")

  is_numbered <- purrr::map_lgl(
    heading_lines,
    \(heading) {
      base::startsWith(heading, prefix_h2) ||
        base::startsWith(heading, prefix_h3)
    }
  )

  heading_count <- base::length(heading_lines)
  numbered_count <- base::sum(is_numbered)
  audit_ok <- heading_count == numbered_count

  tibble::tibble(
    file = file,
    headings = heading_count,
    numbered = numbered_count,
    ok = audit_ok
  )
}

tbl_heading_audit <- purrr::pmap_dfr(
  tbl_chapters,
  \(file, part, chapter) audit_one_file_LFL(file, part, chapter)
)

base::print(
  tbl_heading_audit,
  n = base::nrow(tbl_heading_audit),
  width = Inf
)

if (!base::all(tbl_heading_audit$ok)) {
  base::stop("Heading numbering audit failed.", call. = FALSE)
}

base::cat(
  "\nAll 24 chapters passed the heading audit.\n",
  "No QMD files were modified by this audit script.\n",
  sep = ""
)
