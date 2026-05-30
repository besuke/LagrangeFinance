library(fs)
library(stringr)

files <- dir_ls("chapters_lagrange", regexp = "ch[0-9]+\\.qmd$")

for (f in files) {
  x <- readLines(f, warn = FALSE)

  # 先頭の自動生成タイトル "# Chapter 03" などを削除
  x <- x[!str_detect(x, "^# Chapter [0-9]+$")]

  # 先頭の空行を整理
  while (length(x) > 0 && str_trim(x[1]) == "") {
    x <- x[-1]
  }

  writeLines(x, f)
}

message("Removed artificial English chapter headings.")
