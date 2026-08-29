# LagrangeFinance Part II audit
# Run from project root:
# source("audit_part2_LFL.R")

vec_part2_files <- base::file.path(
  "part2_quantive_riemann",
  base::c(
    "ch01_deterministic_cashflow_valuation.qmd",
    "ch02_interest_rate_curves.qmd",
    "ch03_ois_swaps_multicurve.qmd",
    "ch04_black_model.qmd",
    "ch05_normal_model.qmd",
    "ch06_sabr_model.qmd",
    "ch07_callable_hull_white.qmd",
    "ch08_credit_risk_cds.qmd",
    "ch09_exotic_derivatives_prdc.qmd"
  )
)

if (!base::all(base::file.exists(vec_part2_files))) {
  base::stop(
    "Run this script from the LagrangeFinance project root.",
    call. = FALSE
  )
}

count_pattern_LFL <- function(text, pattern) {
  stringr::str_count(text, pattern) |>
    base::sum()
}

extract_title_LFL <- function(text) {
  title_line <- stringr::str_match(
    text,
    '(?m)^title:\\s*"([^"]+)"\\s*$'
  )[, 2]

  if (base::length(title_line) == 0 || base::is.na(title_line[[1]])) {
    NA_character_
  } else {
    title_line[[1]]
  }
}

tbl_part2_audit <- purrr::map_dfr(
  vec_part2_files,
  \(file_path) {
    text <- readr::read_file(file_path)

    tibble::tibble(
      file = base::basename(file_path),
      title = extract_title_LFL(text),
      n_inf = count_pattern_LFL(text, "n\\s*=\\s*Inf"),
      theme_minimal = count_pattern_LFL(text, "ggplot2::theme_minimal\\s*\\("),
      theme_classic = count_pattern_LFL(text, "ggplot2::theme_classic\\s*\\("),
      naked_print = count_pattern_LFL(
        text,
        "(?m)^\\s*(?:base::)?print\\s*\\("
      ),
      naked_path_expression = count_pattern_LFL(
        text,
        "(?m)^\\s*[A-Za-z0-9_]*(?:path|paths)[A-Za-z0-9_]*\\s*$"
      ),
      windows_absolute_path = count_pattern_LFL(
        text,
        "(?m)(?<!https)(?<!http)[A-Za-z]:[/\\\\]"
      ),
      show_table = count_pattern_LFL(text, "show_table_LFL\\s*\\("),
      show_plot = count_pattern_LFL(text, "show_plot_LFL\\s*\\("),
      add_theme = count_pattern_LFL(text, "add_lagrange_theme_LFL\\s*\\(")
    )
  }
)

base::print(
  tbl_part2_audit,
  n = base::nrow(tbl_part2_audit),
  width = Inf
)

base::cat(
  "\nPart II audit completed.\n",
  "Paste the table above back into ChatGPT.\n",
  sep = ""
)
