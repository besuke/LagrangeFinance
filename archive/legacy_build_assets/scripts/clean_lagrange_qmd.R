library(fs)
library(stringr)

files <- dir_ls("chapters_lagrange", regexp = "\\.qmd$")

for (f in files) {
  x <- readLines(f, warn = FALSE)

  # 自動生成された英語章タイトルを削除
  x <- x[!str_detect(x, "^# Chapter [0-9]+$")]

  # Rファイル由来の「# ch03_01: ...」見出しを削除
  x <- x[!str_detect(x, "^# ch[0-9]+_[0-9a-zA-Z]+:")]

  # 最初の「第n章」を章タイトル # に昇格
  idx <- which(str_detect(x, "^#+\\s*第[0-9０-９]+章"))[1]

  if (!is.na(idx)) {
    title <- str_replace(x[idx], "^#+\\s*", "")
    x[idx] <- paste0("# ", title)
  }

  writeLines(x, f)
}

message("Cleaned LagrangeFinance qmd headings.")
