library(jsonlite)
library(fs)
library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

ipynb_files <- dir_ls("sources/python", regexp = "\\.ipynb$")

extract_nb <- function(path) {
  nb <- fromJSON(path, simplifyVector = FALSE)
  cells <- nb$cells

  tibble(
    chapter = str_extract(path_file(path), "ch[0-9]+"),
    notebook = path_file(path),
    cell_id = seq_along(cells),
    cell_type = map_chr(cells, "cell_type"),
    first_line = map_chr(cells, function(x) {
      src <- paste0(unlist(x$source), collapse = "")
      line <- str_split(src, "\n", simplify = TRUE)[1]
      str_squish(line)
    })
  )
}

nb_index <- map_dfr(ipynb_files, extract_nb)

dir_create("reports")
write_csv(nb_index, "reports/python_notebook_cells.csv")

summary <- nb_index |>
  count(chapter, notebook, cell_type)

write_csv(summary, "reports/python_notebook_summary.csv")

print(summary)
message("Created reports/python_notebook_cells.csv and reports/python_notebook_summary.csv")
