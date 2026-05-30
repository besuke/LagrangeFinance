library(fs)
library(stringr)

files <- dir_ls("sources/r_refactored", regexp = "\\.R$", recurse = TRUE)

to_snake <- function(x) {
  x |>
    str_replace_all("([a-z0-9])([A-Z])", "\\1_\\2") |>
    str_replace_all("\\.", "_") |>
    str_to_lower()
}

for (f in files) {
  x <- readLines(f, warn = FALSE)

  # magrittr pipe -> base pipe
  x <- str_replace_all(x, fixed("%>%"), "|>")

  # common function names
  x <- str_replace_all(x, "\\bcalculate_IRR\\b", "calculate_irr")
  x <- str_replace_all(x, "\\bcalculateIRR\\b", "calculate_irr")
  x <- str_replace_all(x, "\\bcalculate_NPV\\b", "calculate_npv")
  x <- str_replace_all(x, "\\bcalculateNPV\\b", "calculate_npv")

  # function definitions: fooBar <- function(...) -> foo_bar <- function(...)
  fun_lines <- str_detect(x, "^\\s*[A-Za-z][A-Za-z0-9._]*\\s*<-\\s*function\\s*\\(")

  if (any(fun_lines)) {
    old_names <- str_match(x[fun_lines], "^\\s*([A-Za-z][A-Za-z0-9._]*)\\s*<-\\s*function")[, 2]
    new_names <- to_snake(old_names)

    for (i in seq_along(old_names)) {
      if (!is.na(old_names[i]) && old_names[i] != new_names[i]) {
        x <- str_replace_all(x, paste0("\\b", old_names[i], "\\b"), new_names[i])
      }
    }
  }

  writeLines(x, f)
}

message("Refactored R sources into sources/r_refactored")
