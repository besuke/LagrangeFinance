library(jsonlite)
library(fs)
library(stringr)
library(purrr)

dir_create("chapters_lagrange")

make_chapter <- function(ch) {
  ipynb <- path("sources/python", paste0(ch, ".ipynb"))
  r_files <- dir_ls("sources/r", regexp = paste0(ch, "_.*\\.R$"), recurse = TRUE)
  r_files <- sort(r_files)

  nb <- fromJSON(ipynb, simplifyVector = FALSE)
  cells <- nb$cells

  r_i <- 1
  out <- character()

  out <- c(out, paste0("# Chapter ", str_remove(ch, "ch")), "")

  for (cell in cells) {
    src <- paste0(unlist(cell$source), collapse = "")

    if (cell$cell_type == "markdown") {
      out <- c(out, src, "")
    }

    if (cell$cell_type == "code") {
      if (r_i <= length(r_files)) {
        code <- readLines(r_files[[r_i]], warn = FALSE)

        out <- c(
          out,
          "```{r}",
          "#| message: false",
          "#| warning: false",
          code,
          "```",
          ""
        )
      } else {
        out <- c(
          out,
          "```{r}",
          "# R code not found for this Python cell",
          "```",
          ""
        )
      }

      r_i <- r_i + 1
    }
  }

  writeLines(out, path("chapters_lagrange", paste0(ch, ".qmd")))
  message(ch, ": used ", min(r_i - 1, length(r_files)), " R files / ", length(r_files), " available")
}

walk(sprintf("ch%02d", 3:8), make_chapter)
