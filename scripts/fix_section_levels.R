library(fs)
library(stringr)

files <- dir_ls("chapters_edited", regexp = "ch[0-9]+\\.qmd$")

for (f in files) {
  x <- readLines(f, warn = FALSE)

  # 第n章 は必ず # にする
  x <- str_replace(
    x,
    "^#+\\s*(第[0-9０-９]+章.*)$",
    "# \\1"
  )

  # 3.1, 5.1, 8.3 などは必ず ## にする
  x <- str_replace(
    x,
    "^#+\\s*([0-9]+\\.[0-9]+\\s+.*)$",
    "## \\1"
  )

  # 3.1.1, 5.6.2 などは必ず ### にする
  x <- str_replace(
    x,
    "^#+\\s*([0-9]+\\.[0-9]+\\.[0-9]+\\s+.*)$",
    "### \\1"
  )

  writeLines(x, f)
}

message("Fixed section heading levels.")
