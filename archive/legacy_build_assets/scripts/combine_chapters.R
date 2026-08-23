library(fs)
library(stringr)

dir_create("chapters_combined")

for (ch in sprintf("ch%02d", 3:8)) {
  files <- dir_ls("chapters", regexp = paste0(ch, "_.*\\.qmd$"))
  files <- sort(files)

  if (length(files) == 0) next

  out <- path("chapters_combined", paste0(ch, ".qmd"))

  body <- unlist(lapply(files, function(f) {
    x <- readLines(f, warn = FALSE)
    c("", paste0("## ", path_ext_remove(path_file(f))), "", x[-1])
  }))

  title <- c(
    paste0("# Chapter ", str_extract(ch, "[0-9]+")),
    "",
    body
  )

  writeLines(title, out)
}

message("Created combined chapters in chapters_combined/")
