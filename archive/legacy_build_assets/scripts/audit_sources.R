library(fs)
library(tibble)
library(stringr)
library(readr)
library(dplyr)

python_files <- dir_ls("sources/python", regexp = "\\.ipynb$") |>
  tibble(path = _) |>
  mutate(
    source = "python",
    file = path_file(path),
    chapter = str_extract(file, "ch[0-9]+")
  )

r_files <- dir_ls("sources/r", regexp = "\\.R$", recurse = TRUE) |>
  tibble(path = _) |>
  mutate(
    source = "r",
    file = path_file(path),
    chapter = str_extract(file, "ch[0-9]+")
  )

audit <- bind_rows(python_files, r_files) |>
  arrange(chapter, source, file)

dir_create("reports")

write_csv(audit, "reports/source_inventory.csv")

summary_tbl <- audit |>
  count(chapter, source) |>
  tidyr::pivot_wider(
    names_from = source,
    values_from = n,
    values_fill = 0
  )

write_csv(summary_tbl, "reports/source_summary.csv")

print(summary_tbl)
message("Created reports/source_inventory.csv and reports/source_summary.csv")
