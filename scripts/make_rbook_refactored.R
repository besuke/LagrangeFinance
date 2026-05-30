library(jsonlite)
library(fs)
library(stringr)
library(purrr)

dir_create("chapters_refactored")

clean_r_code <- function(code) {
  code |>
    str_subset("^# ch[0-9]+_[0-9A-Za-z]+:", negate = TRUE) |>
    str_subset("^## \\[", negate = TRUE)
}

fix_heading <- function(lines) {
  lines |>
    str_replace("^#+\\s*(第[0-9０-９]+章.*)$", "# \\1") |>
    str_replace("^#+\\s*([0-9]+\\.[0-9]+\\s+.*)$", "## \\1") |>
    str_replace("^#+\\s*([0-9]+\\.[0-9]+\\.[0-9]+\\s+.*)$", "### \\1")
}

make_chapter <- function(ch) {
  ipynb <- path("sources/python", paste0(ch, ".ipynb"))
  r_files <- sort(dir_ls("sources/r_refactored", regexp = paste0(ch, "_.*\\.R$"), recurse = TRUE))

  nb <- fromJSON(ipynb, simplifyVector = FALSE)
  cells <- nb$cells

  out <- character()
  r_i <- 1

  for (cell in cells) {
    src <- paste0(unlist(cell$source), collapse = "")

    if (cell$cell_type == "markdown") {
      lines <- str_split(src, "\n")[[1]]
      lines <- fix_heading(lines)
      out <- c(out, lines, "")
    }

    if (cell$cell_type == "code") {
      if (r_i <= length(r_files)) {
        code <- readLines(r_files[[r_i]], warn = FALSE)
        code <- clean_r_code(code)

        out <- c(
          out,
          "```{r}",
          "#| eval: false",
          "#| message: false",
          "#| warning: false",
          code,
          "```",
          ""
        )
      }
      r_i <- r_i + 1
    }
  }

  writeLines(out, path("chapters_refactored", paste0(ch, ".qmd")))
  message(ch, ": used ", min(r_i - 1, length(r_files)), " / ", length(r_files), " R files")
}

walk(sprintf("ch%02d", 3:8), make_chapter)
