library(fs)
library(stringr)
library(purrr)

dir_create("chapters_from_r")

title_map <- c(
  ch03 = "第3章 Rの導入",
  ch04 = "第4章 財務データの取得と可視化",
  ch05 = "第5章 株式データの取得と可視化",
  ch06 = "第6章 ファクター・モデルの導入",
  ch07 = "第7章 ファクター・モデルの応用",
  ch08 = "第8章 イベント・スタディ"
)

make_chapter <- function(ch) {
  r_files <- dir_ls("sources/r", regexp = paste0(ch, "_.*\\.R$"), recurse = TRUE)
  r_files <- sort(r_files)

  out <- c(paste0("# ", title_map[[ch]]), "")

  for (f in r_files) {
    code <- readLines(f, warn = FALSE)

    header <- code[str_detect(code, "^# ch[0-9]+_")]
    if (length(header) > 0) {
      section_title <- str_replace(header[1], "^#\\s*", "")
      section_title <- str_replace(section_title, "^ch[0-9]+_[0-9A-Za-z]+:\\s*", "")
      out <- c(out, paste0("## ", section_title), "")
    }

    code <- code[!str_detect(code, "^# ch[0-9]+_")]
    code <- code[!str_detect(code, "^## \\[")]

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

  writeLines(out, path("chapters_from_r", paste0(ch, ".qmd")))
  message(ch, ": ", length(r_files), " R files")
}

walk(sprintf("ch%02d", 3:8), make_chapter)
