# normalize_part3_ch05_ch06_headings_LFL.R
#
# 現在の ch05 / ch06 の状態から、トップレベル見出しを合意済み構成へ正規化する。
# 本文・数式・Rコード・図表は変更しない。
#
# 実行:
# setwd("C:/AnalyticFin/Projects/LagrangeFinance")
# source("normalize_part3_ch05_ch06_headings_LFL.R")

root <- getwd()
dir3 <- file.path(root, "part3_enterprise_riemann")

f5 <- file.path(dir3, "ch05_option_greeks_autodiff.qmd")
f6 <- file.path(dir3, "ch06_interest_rate_risk_autodiff.qmd")

if (!file.exists(f5)) stop("Missing: ", f5)
if (!file.exists(f6)) stop("Missing: ", f6)

# ------------------------------------------------------------
# 共通関数
# ------------------------------------------------------------

set_heading <- function(x, pattern, replacement) {
  idx <- grep(pattern, x, perl = TRUE)

  if (length(idx) == 1L) {
    x[idx] <- replacement
  } else if (length(idx) > 1L) {
    stop("Multiple matches: ", pattern)
  }

  x
}

insert_before <- function(x, pattern, new_line) {
  idx <- grep(pattern, x, perl = TRUE)

  if (length(idx) != 1L) {
    stop("Cannot determine insertion point: ", pattern)
  }

  append(x, new_line, after = idx - 1L)
}

top_headings <- function(x, chapter) {
  grep(
    paste0("^## III-", chapter, "\\."),
    x,
    value = TRUE
  )
}

# ============================================================
# 第5章
# ============================================================

x <- readLines(f5, encoding = "UTF-8", warn = FALSE)

# 1～3 は現在すでに正しいので固定
x <- set_heading(
  x,
  "^## III-5\\.1 ",
  "## III-5.1 オプション評価とリスク感応度"
)

x <- set_heading(
  x,
  "^## III-5\\.2 ",
  "## III-5.2 ブラック・ショールズ式による解析解"
)

x <- set_heading(
  x,
  "^## III-5\\.3 ",
  "## III-5.3 モンテカルロ法における自動微分のリスク感応度"
)

# 以前の処理で III-5.4 が ### に落ちている場合、
# 「微分不可なオプションペイオフのケーススタディ」をトップレベルへ戻す。
idx_case <- grep(
  "^#{2,4} III-5\\.4[^ ]* .*微分不可.*ペイオフ|^#{2,4} III-5\\.4 微分不可",
  x,
  perl = TRUE
)

if (length(idx_case) >= 1L) {
  # 最初の該当見出しをトップレベル 5.4 とする
  x[idx_case[1]] <- "## III-5.4 微分不可なオプションペイオフのケーススタディ"
} else {
  # 現在の 5.3 の後半に旧「微分不可なペイオフ」が残っていればそこを利用
  idx_old <- grep(
    "^### .*微分不可なオプションペイオフ|^### .*微分不可なペイオフ",
    x,
    perl = TRUE
  )

  if (length(idx_old) >= 1L) {
    x[idx_old[1]] <- "## III-5.4 微分不可なオプションペイオフのケーススタディ"
  } else {
    stop("Chapter 5: case-study heading was not found.")
  }
}

# 5.4 より後に残るトップレベル見出しはすべて 5.4 の下位節へ。
# 本文はそのまま。
idx_54 <- grep(
  "^## III-5\\.4 微分不可なオプションペイオフのケーススタディ$",
  x
)

idx_after <- which(
  seq_along(x) > idx_54 &
    grepl("^## III-5\\.", x)
)

if (length(idx_after) > 0L) {
  n <- 1L

  for (i in idx_after) {
    title <- sub("^## III-5\\.[0-9]+\\s+", "", x[i])
    x[i] <- paste0("### III-5.4.", n, " ", title)
    n <- n + 1L
  }
}

writeLines(x, f5, useBytes = TRUE)

# ============================================================
# 第6章
# ============================================================

x <- readLines(f6, encoding = "UTF-8", warn = FALSE)

# 1,2,4 は現在のログ上存在する
x <- set_heading(
  x,
  "^## III-6\\.1 ",
  "## III-6.1 金利スワップ評価とリスク感応度"
)

x <- set_heading(
  x,
  "^## III-6\\.2 ",
  "## III-6.2 金利リスクにおける自動微分"
)

x <- set_heading(
  x,
  "^## III-6\\.4 ",
  "## III-6.4 バケットDV01とGPS"
)

# 前回誤って ### III-6.2.1 に落ちた
# 「イールドカーブと自動微分」を III-6.3 に戻す。
idx_curve <- grep(
  "^#{2,4} III-6\\.[0-9.]+ .*イールドカーブ.*自動微分|^#{2,4} III-6\\.[0-9.]+ .*多変数金利感応度",
  x,
  perl = TRUE
)

if (length(idx_curve) >= 1L) {
  # 6.4 より前にある最初の適切な候補
  idx_64 <- grep("^## III-6\\.4 ", x)

  candidates <- idx_curve[idx_curve < idx_64]

  if (length(candidates) == 0L) {
    stop("Chapter 6: curve section candidate not found before III-6.4.")
  }

  x[candidates[length(candidates)]] <-
    "## III-6.3 イールドカーブと自動微分"
} else {
  stop("Chapter 6: yield-curve heading was not found.")
}

# 6.3 と 6.4 の間にトップレベルがあれば 6.3 配下へ
idx_63 <- grep("^## III-6\\.3 ", x)
idx_64 <- grep("^## III-6\\.4 ", x)

between <- which(
  seq_along(x) > idx_63 &
    seq_along(x) < idx_64 &
    grepl("^## III-6\\.", x)
)

if (length(between) > 0L) {
  n <- 1L

  for (i in between) {
    title <- sub("^## III-6\\.[0-9]+\\s+", "", x[i])
    x[i] <- paste0("### III-6.3.", n, " ", title)
    n <- n + 1L
  }
}

# 6.4 より後に残るトップレベルは 6.4 配下へ
idx_64 <- grep("^## III-6\\.4 ", x)

after <- which(
  seq_along(x) > idx_64 &
    grepl("^## III-6\\.", x)
)

if (length(after) > 0L) {
  n <- 1L

  for (i in after) {
    title <- sub("^## III-6\\.[0-9]+\\s+", "", x[i])
    x[i] <- paste0("### III-6.4.", n, " ", title)
    n <- n + 1L
  }
}

writeLines(x, f6, useBytes = TRUE)

# ============================================================
# 最終監査
# ============================================================

expected5 <- c(
  "## III-5.1 オプション評価とリスク感応度",
  "## III-5.2 ブラック・ショールズ式による解析解",
  "## III-5.3 モンテカルロ法における自動微分のリスク感応度",
  "## III-5.4 微分不可なオプションペイオフのケーススタディ"
)

expected6 <- c(
  "## III-6.1 金利スワップ評価とリスク感応度",
  "## III-6.2 金利リスクにおける自動微分",
  "## III-6.3 イールドカーブと自動微分",
  "## III-6.4 バケットDV01とGPS"
)

check_file <- function(path, chapter, expected) {
  y <- readLines(path, encoding = "UTF-8", warn = FALSE)
  actual <- top_headings(y, chapter)

  cat("\n", basename(path), "\n", sep = "")
  cat(paste0("  ", actual, collapse = "\n"), "\n")

  if (!identical(actual, expected)) {
    stop(
      "\nHeading audit failed in ",
      basename(path),
      "\n\nExpected:\n",
      paste(expected, collapse = "\n"),
      "\n\nActual:\n",
      paste(actual, collapse = "\n")
    )
  }
}

check_file(f5, "5", expected5)
check_file(f6, "6", expected6)

cat("\n========================================\n")
cat("Chapter 5 / Chapter 6 normalization OK\n")
cat("========================================\n")
