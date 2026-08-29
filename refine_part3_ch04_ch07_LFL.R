# refine_part3_ch04_ch07_LFL.R
# Part III ch04-ch07: agreed title/heading refinements only.
# R code, formulas and body prose are not rewritten.

files <- c(
  "part3_enterprise_riemann/ch04_automatic_differentiation.qmd",
  "part3_enterprise_riemann/ch05_option_greeks_autodiff.qmd",
  "part3_enterprise_riemann/ch06_interest_rate_risk_autodiff.qmd",
  "part3_enterprise_riemann/ch07_prdc_autodiff.qmd"
)
base::stopifnot(base::all(base::file.exists(files)))

replace_one <- function(x, old, new) {
  i <- base::which(x == old)
  if (base::length(i) == 1L) x[i] <- new
  x
}
insert_before <- function(x, anchor, line) {
  i <- base::which(x == anchor)
  if (base::length(i) == 1L && !base::any(x == line))
    x <- base::append(x, line, after = i - 1L)
  x
}

# III-4
f <- files[1]; x <- base::readLines(f, encoding="UTF-8", warn=FALSE)
pairs <- list(
c('title: "第III-4章 自動微分と微分可能なプライシング"','title: "第III-4章 自動微分による市場リスク感応度計測"'),
c("## III-4.1 金融リスク感応度と偏微分","## III-4.1 市場リスク感応度と偏微分"),
c("## III-4.2 有限差分とバンプ・アンド・リバリュー","## III-4.2 変化幅による再評価と有限差分"),
c("### III-4.3.2 計算グラフ","### III-4.3.2 コンピュテーション・グラフ"),
c("### III-4.4.1 リバースモード","### III-4.4.2 リバースモード"),
c("## III-4.5 AADと金融工学","## III-4.5 自動微分と金融工学"),
c("## III-4.6 R torchによる自動微分の実装","## III-4.6 深層学習ライブラリによる自動微分"),
c("## III-4.10 二変数関数のGradient","### III-4.6.2 多変数関数の偏微分"),
c("## III-4.11 金融リスク計測への接続","## III-4.7 自動微分による市場リスク感応度"),
c("## III-4.12 まとめ","## III-4.8 まとめ"))
for (z in pairs) x <- replace_one(x,z[1],z[2])
x <- insert_before(x,"Forward Modeでは、入力側から出力側へ向かって、","### III-4.4.1 フォワードモード")
x <- insert_before(x,"ここから、Rの`torch`を使って実際に自動微分を確認する。","### III-4.6.1 一変数関数の自動微分")
base::writeLines(x,f,useBytes=TRUE); base::cat("Updated:",f,"\n")

# III-5
f <- files[2]; x <- base::readLines(f, encoding="UTF-8", warn=FALSE)
pairs <- list(
c('title: "第III-5章 オプション・グリークス：GaussQuantとモンテカルロ・torchの比較"','title: "第III-5章 解析解とモンテカルロ自動微分による検証"'),
c("## III-5.1 計算条件","## III-5.1 オプション評価の計算条件"),
c("## III-5.2 GaussQuantによる基準価格とグリークス","## III-5.2 解析解による価格と感応度"),
c("## III-5.3 微分可能なモンテカルロ評価","## III-5.3 モンテカルロによる価格評価"),
c("### III-5.3.3 torchテンソルの設定","### III-5.3.3 深層学習ライブラリによるテンソル計算"),
c("### III-5.3.4 モンテカルロNPV","### III-5.3.4 モンテカルロによる現在価値"),
c("## III-5.4 一次グリークスの自動微分","## III-5.4 自動微分による一次感応度"),
c("## III-5.5 ガンマと非滑らかなペイオフ","## III-5.5 微分不可なペイオフのケース"),
c("### III-5.5.1 平滑化ペイオフによるガンマ","### III-5.5.1 平滑化ペイオフ"),
c("## III-5.6 GaussQuantとモンテカルロ・torchの比較","## III-5.6 解析解とモンテカルロ自動微分の比較"))
for (z in pairs) x <- replace_one(x,z[1],z[2])
x <- insert_before(x,"GaussQuantでは、European OptionをQuantLibオブジェクトとして構築できる。","### III-5.2.1 GaussQuantによる解析評価")
x <- insert_before(x,"さらにDeltaをSpotで微分する。","### III-5.5.2 自動微分によるガンマ")
base::writeLines(x,f,useBytes=TRUE); base::cat("Updated:",f,"\n")

# III-6
f <- files[3]; x <- base::readLines(f, encoding="UTF-8", warn=FALSE)
pairs <- list(
c('title: "第III-6章 金利リスク：金利感応度からカーブ・グラディエントへ"','title: "第III-6章 自動微分による多変数金利感応度の計測"'),
c("## III-6.1 金利商品の現在価値","## III-6.1 金利商品の現在価値と感応度"),
c("### III-6.1.1 固定レッグ","### III-6.1.1 固定レグ"),
c("### III-6.1.2 変動レッグ","### III-6.1.2 変動レグ"),
c("### III-6.1.3 スワップNPV","### III-6.1.3 スワップの現在価値"),
c("## III-6.2 金利感応度","## III-6.2 単一金利に対する感応度"),
c("## III-6.3 torchによるスワップ評価","## III-6.3 深層学習ライブラリによるスワップ評価"),
c("## III-6.6 フラット金利からイールドカーブへ","## III-6.6 多変数金利感応度への拡張"),
c("### III-6.6.1 カーブ・ノードのテンソル化","### III-6.6.1 イールドカーブのテンソル化"),
c("## III-6.7 Bucketed DV01","## III-6.7 バケットDV01とGPS"),
c("## III-6.7 Bucketed DV01とGPS","## III-6.7 バケットDV01とGPS"),
c("## III-6.7 バケットDV01","## III-6.7 バケットDV01とGPS"))
for (z in pairs) x <- replace_one(x,z[1],z[2])
base::writeLines(x,f,useBytes=TRUE); base::cat("Updated:",f,"\n")

# III-7
f <- files[4]; x <- base::readLines(f, encoding="UTF-8", warn=FALSE)
pairs <- list(
c('title: "第III-7章 金利為替系デリバティブ：多因子モンテカルロと自動微分"','title: "第III-7章 多因子モンテカルロの自動微分による市場リスク感応度計測"'),
c("## III-7.1 金利為替系デリバティブのモデル構造","## III-7.1 多因子市場モデル"),
c("### III-7.1.1 為替連動クーポン","### III-7.1.2 為替連動クーポン"),
c("### III-7.1.2 為替のリスク中立過程","### III-7.1.3 為替のリスク中立過程"),
c("### III-7.2.2 torchによる為替パス生成","### III-7.2.2 深層学習ライブラリによる為替パス生成"),
c("### III-7.2.3 微分可能なプライシング関数","### III-7.2.3 微分可能なプライシング"),
c("### III-7.2.4 NPVの計算","### III-7.2.4 モンテカルロによる現在価値"),
c("## III-7.3 自動微分による市場リスク計測","## III-7.3 自動微分による多因子感応度"),
c("## III-7.4 有限差分法による感応度の検証","## III-7.4 有限差分法による検証"),
c("## III-7.5 非線形ペイオフと自動微分","## III-7.5 微分不可なペイオフのケース"),
c("### III-7.5.1 モンテカルロと自動微分","### III-7.5.2 微分不可なペイオフの感応度"),
c("## III-7.6 大規模リスク計測への展開","## III-7.6 大規模市場リスク計測への展開"),
c("### III-7.6.1 AADによる統合リスク計測","### III-7.6.2 自動微分による多因子感応度の一括計算"),
c("### III-7.6.2 第III部を通したリスク計測の発展","### III-7.6.3 エンタープライズ・リスク計算への展開"))
for (z in pairs) x <- replace_one(x,z[1],z[2])
x <- insert_before(x,"PRDCは、為替と複数通貨の金利を同時に扱うハイブリッド商品である。","### III-7.1.1 市場リスクファクター")
base::writeLines(x,f,useBytes=TRUE); base::cat("Updated:",f,"\n")

base::cat("\nCompleted: Part III ch04-ch07 heading refinement.\n")
