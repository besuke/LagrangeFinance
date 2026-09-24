# fix_part3_ch05_ch06_exact_LFL.R
# 現在確認済みの見出し構造に対する直接修正版

root <- getwd()
dir3 <- file.path(root, "part3_enterprise_riemann")
f5 <- file.path(dir3, "ch05_option_greeks_autodiff.qmd")
f6 <- file.path(dir3, "ch06_interest_rate_risk_autodiff.qmd")

# ---- Chapter 5 ----
x <- readLines(f5, encoding = "UTF-8", warn = FALSE)

# 誤って Markdown 見出しになっているコードコメントを R コメントへ戻す
x[x == "## maturity は残存期間 T なので、"] <- "# maturity は残存期間 T なので、"
x[x == "## Calendar-Time Theta は -dV/dT とする。"] <- "# Calendar-Time Theta は -dV/dT とする。"

# 重複した一次感応度見出しは、後ろ側だけ削除
idx <- which(x == "### III-5.3.5 自動微分による一次感応度")
if (length(idx) == 2L) x <- x[-idx[2L]]

# 5.4 のトップレベル見出しを 5.4.1 の直前に復元
anchor <- which(x == "### III-5.4.1 平滑化ペイオフ")
if (length(anchor) != 1L) stop("Chapter 5 anchor not found.")

if (!any(x == "## III-5.4 微分不可なオプションペイオフのケーススタディ")) {
  x <- append(
    x,
    "## III-5.4 微分不可なオプションペイオフのケーススタディ",
    after = anchor - 1L
  )
}

writeLines(x, f5, useBytes = TRUE)

# ---- Chapter 6 ----
x <- readLines(f6, encoding = "UTF-8", warn = FALSE)

# 誤って 6.2 配下へ落ちたイールドカーブ節をトップレベル 6.3 に戻す
x[x == "### III-6.2.1 イールドカーブと自動微分"] <-
  "## III-6.3 イールドカーブと自動微分"

writeLines(x, f6, useBytes = TRUE)

# ---- Audit ----
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

y5 <- readLines(f5, encoding = "UTF-8", warn = FALSE)
y6 <- readLines(f6, encoding = "UTF-8", warn = FALSE)

actual5 <- grep("^## III-5\\.", y5, value = TRUE)
actual6 <- grep("^## III-6\\.", y6, value = TRUE)

cat("\nChapter 5\n")
cat(paste0("  ", actual5, collapse = "\n"), "\n")

cat("\nChapter 6\n")
cat(paste0("  ", actual6, collapse = "\n"), "\n")

stopifnot(identical(actual5, expected5))
stopifnot(identical(actual6, expected6))

cat("\nAUDIT OK\n")
