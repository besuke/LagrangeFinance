library(fs)
library(stringr)

files <- dir_ls("chapters_lagrange", regexp = "ch[0-9]+\\.qmd$")

for (f in files) {
  x <- readLines(f, warn = FALSE)

  # 自動英語タイトルを削除
  x <- x[!str_detect(x, "^# Chapter [0-9]+$")]

  # 「第n章」を含む最初の見出しを # にする
  idx <- which(str_detect(x, "^#+\\s*第[0-9０-９]+章"))[1]

  if (!is.na(idx)) {
    title <- str_replace(x[idx], "^#+\\s*", "")
    x[idx] <- paste0("# ", title)
  }

  writeLines(x, f)
}

message("Promoted Japanese chapter headings.")
