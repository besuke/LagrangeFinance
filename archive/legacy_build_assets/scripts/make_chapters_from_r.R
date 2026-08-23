library(fs)
library(stringr)

dir_create("chapters")

r_files <- dir_ls("sources/r", regexp = "ch[0-9]+.*\\.R$", recurse = TRUE)

if (length(r_files) == 0) {
  stop("R source files were not found in sources/r")
}

for (f in r_files) {
  base <- path_file(f)
  stem <- path_ext_remove(base)
  out <- path("chapters", paste0(stem, ".qmd"))

  code <- readLines(f, warn = FALSE)
  title <- paste("Chapter", str_extract(stem, "[0-9]+"))

  qmd <- c(
    paste0("# ", title),
    "",
    "```{r}",
    "#| message: false",
    "#| warning: false",
    code,
    "```"
  )

  writeLines(qmd, out)
}

message("Created Quarto chapters in chapters/")
