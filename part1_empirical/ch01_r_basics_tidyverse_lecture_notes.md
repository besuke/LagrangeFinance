# 講師用メモ：第I-1章 Rによるデータ分析の基礎

元ファイル：`ch01_r_basics_tidyverse.qmd`（このメモは講義中の手元確認用。本文の補足ではない）

## 講義前の確認

- 作業ディレクトリはプロジェクトルート。冒頭チャンクが `source(file.path("R", "utility_LFL.R"))` で tidyverse と `*_LFL` 関数を読み込む
- `data/raw/market/daily_stock_returns.parquet` があること（I-1.2.1 以降で使う）
- `arrow`、`furrr`、`future` が入っていること
- 本章の方針：明示的なループは使わない（ベクトル化か `map()` 系）、パイプは `|>` に統一
- 注意：I-1.1.2・I-1.1.4 で、tidyverse 紹介前に `tibble()`・`mutate()` が出てくる。「後で説明する」と一言添える

---

## I-1.1 Rの基礎

### I-1.1.1 Rの基本操作

- 要点
  - 代入は `<-`、オブジェクト名は snake_case
  - 型：numeric / integer（`10L`）/ character / logical。`class()`・`typeof()`・`str()` で確認
  - 欠損は `NA`。集計関数は `na.rm = TRUE` を指定しないと結果が `NA` になる
- 実演
  - `present_value <- future_cash_flow / (1 + risk_free_rate)`
  - `mean(returns_with_na, na.rm = TRUE)`
- つまずき
  - `=` と `<-` の違い（関数の引数指定は `=`）
  - `10` と `10L` の違い。`class()` と `typeof()` で表示が違う
  - `NA` を含むと `mean()` が `NA` になる

### I-1.1.2 ベクトル

- 要点
  - 作り方：`c()`・`:`・`seq()`・`rep()`
  - 添字は 1 から。負の添字は「除く」。条件で抽出できる
  - 名前付きベクトル。計算はベクトル全体に一度に適用される（ベクトル化）
- 実演
  - `interest_rates[interest_rates >= 0.015]`
  - `cash_flow / (1 + discount_rates)` → `tibble(discount_rates, present_values)`
- つまずき
  - 添字の 0 始まり（Python・C++ 経験者）。`[-1]` は「最後」ではなく「1番目を除く」
  - `seq(0, 1, by = 0.25)` と `1:5` の違い

### I-1.1.3 行列、リスト、データフレーム

- 要点
  - 行列：`matrix(..., byrow = TRUE)`、行名・列名、`colMeans()`
  - リスト：型・長さの違う要素をまとめる。`$`・`[[ ]]` で取り出す
  - `data.frame` → `tibble`（表示が見やすく、型が保たれる）、`tribble()` は行単位で書く
- 実演
  - `return_matrix[, "stock_a"]`
  - `analysis_result[["observations"]]`
  - `tribble(~project, ~initial_cost, ~cash_flow, ...)`
- つまずき
  - リストの `[ ]` はリストのまま、`[[ ]]` は中身を返す
  - `byrow` を忘れると数値の並びが変わる

### I-1.1.4 条件判定と独自関数

- 要点
  - スカラーの分岐は `if / else`、ベクトルの多分岐は `case_when()`
  - 関数は `function(引数, 既定値)`。ベクトルを受け取れるように作ると再利用しやすい
- 実演
  - `case_when(score >= 80 ~ "A", score >= 65 ~ "B", TRUE ~ "C")`
  - `calculate_present_value(c(100, 100, 100), 0.02, years = 1:3)`
- つまずき
  - `if` にベクトルを渡すとエラー（長さ 1 の条件しか取れない）
  - `case_when()` は上から評価され、最初に当たった条件が使われる。`TRUE ~` は「その他」
  - `calculate_present_value()` は I-1.3.1 で再利用する。消さないよう注意

### I-1.1.5 記述統計

- 要点
  - `mean`・`median`・`var`・`sd`・`quantile`、`cov`・`cor`、`rank`・`order`
- 実演
  - `quantile(sample_returns)`
  - `rank(-firm_returns)` と `order(firm_returns, decreasing = TRUE)` を並べて見せる
- つまずき
  - `rank()` は各要素の順位、`order()` は並べ替えの添字。混同しやすい
  - `var()`・`sd()` は不偏推定（n − 1 で割る）

### I-1.1.6 確率分布

- 要点
  - 接頭辞の規則：`r` 乱数、`d` 密度、`p` 累積確率、`q` 分位点
  - 一様・正規・二項・ポアソン。最後に関数の対応表（tribble）
  - 金融の例：`pnorm()` で損失確率、`qnorm(0.05, ...)` で 95% VaR に相当する分位点
- 実演
  - `pnorm(1.96)`、`qnorm(0.975)`（往復の関係を見せる）
  - `qnorm(0.05, mean = 0.01, sd = 0.03)`
- つまずき
  - `set_random_seed_LFL()` を飛ばすと、乱数の結果がテキストと合わない
  - 離散分布の `p`（以下の確率）と `q` の結果の解釈
  - VaR の符号（分位点はリターンの値。損失額として言うときは符号を反転する）

---

## I-1.2 tidyverseによるデータ整形

### I-1.2.1 プロジェクト内のデータを読み込む

- 要点
  - データは `data/raw`（外部入力）と `data/derived`（加工結果）に分ける
  - パス探索は `utility_LFL.R` に集約。各章では相対パスだけを書く
  - 読込み直後に、必要な列があるかを確認する
- 実演
  - `read_parquet_file_LFL(file.path("raw", "market", "daily_stock_returns.parquet"))`
  - `setdiff(vec_required_columns, names(...))`
- つまずき
  - 作業ディレクトリがルートでないとファイルが見つからない
  - 絶対パスを書きたくなる。書かない理由（環境が変わると動かない）を説明する

### I-1.2.2 dplyrによるデータ操作

- 要点
  - `select`・`filter`・`mutate`・`arrange`・`summarise`・`group_by`・`count`・`left_join`
- 実演
  - `mutate(market_direction = case_when(...))`
  - `group_by(market_direction) |> summarise(..., .groups = "drop")`
  - `pivot_longer()` してから `left_join(firm_master, by = "firm")`
- つまずき
  - `filter()` のカンマは AND
  - `group_by()` 後のグループが残り、後の処理に影響する（`.groups = "drop"` か `ungroup()`）
  - `left_join()` のキー名・型が合わないと NA になる
  - join の例で `pivot_longer()`（I-1.2.4）を先に使っている

### I-1.2.3 stringrによる文字列処理

- 要点
  - `str_` で始まる関数群：`str_squish`・`str_to_lower`・`str_length`・`str_detect`・`str_extract`・`str_remove`・`str_c`
- 実演
  - `str_detect(clean_name, regex("group", ignore_case = TRUE))`
  - `str_remove(clean_name, regex(" co\\., ltd\\.$", ignore_case = TRUE))`
- つまずき
  - 正規表現のエスケープは `\\.` と二重にする
  - `str_trim()` と `str_squish()` の違い（後者は内部の連続空白も 1 つにまとめる）

### I-1.2.4 tidyrによるデータ整形

- 要点
  - 横持ちと縦持ち：`pivot_longer()` / `pivot_wider()`
  - 列の分割：`separate_wider_delim()`
  - 欠損の処理：`replace_na()` / `drop_na()`
- 実演
  - `pivot_longer(cols = c(firm1, firm2), names_to = "firm", values_to = "return")`
- つまずき
  - `names_to` / `values_to` は文字列で指定する
  - 古い資料の `gather` / `spread` / `separate` との違い
  - 欠損を 0 で埋めることの意味（リターンを 0 とみなしてよいか）

### I-1.2.5 ggplot2による可視化

- 要点
  - `ggplot()` にデータ → `aes()` で対応付け → `geom_*()` で図形 → `labs()`
  - `facet_wrap()`、`linetype` による系列の区別、本書の標準テーマ `add_lagrange_theme_LFL()`
- 実演
  - 折れ線・ヒストグラム・密度・`dnorm` / `pnorm` の曲線、乱数分布の比較
- つまずき
  - ggplot の層は `+` でつなぐ（`|>` ではない）
  - 本書の `( ... ) |> add_lagrange_theme_LFL()` という括弧の書き方。括弧を外すと動かない
  - `scales = "free"` の有無で見え方が変わる

---

## I-1.3 関数型プログラミングと並列計算

### I-1.3.1 purrrによる反復処理

- 要点
  - `map()` はリストを返す。型を指定するなら `map_dbl()` など。複数引数は `map2()` / `pmap()`
  - 行ごとに表を作って結合する（`map_dfr`）。副作用だけが目的なら `walk()`
  - 無名関数 `\(x)`
- 実演
  - `discount_rates |> map_dbl(\(rate) calculate_present_value(100, rate))`
  - `pmap_dbl(list(cash_flow, discount_rate, years), ...)`
  - tribble の列に関数を入れて `map()` で呼ぶ（分布のシミュレーション）
- つまずき
  - `map()` の結果がリストになっていることに気づかない
  - `pmap()` で、リストの名前と関数の引数名を合わせる
  - `map_dfr()` は purrr 1.0 で非推奨扱い（`map() |> list_rbind()` に移行）。質問されたときに補足する

### I-1.3.2 nestによるグループ別データ管理

- 要点
  - `nest(tbl_data = c(date, return), .by = firm)` でグループごとのデータをリスト列に入れる
  - `map()` で各グループに同じ処理 → `unnest()`
  - `group_by` は集計単位、`nest` はデータそのものを保持する
- 実演
  - `tbl_nested$tbl_data[[1]]`
  - `mutate(tbl_summary = map(tbl_data, ...)) |> unnest(tbl_summary)`
- つまずき
  - リスト列の表示（`<tibble [n × 2]>`）が分かりにくい
  - `unnest()` を忘れて、結果がリスト列のまま残る

### I-1.3.3 futureとfurrrによる並列計算

- 要点
  - `plan(multisession)`（Windows）で `future_map()` が並列になる。記法は `map()` とほぼ同じ
  - モンテカルロ法で円周率を推定し、逐次処理と並列処理の時間を比べる
  - 起動・転送のオーバーヘッドがあるので、処理が軽いと逐次処理のほうが速いこともある
- 実演
  - `future::plan(future::multisession, workers = int_workers)`
  - `future_map(..., .options = furrr::furrr_options(seed = TRUE))`
- つまずき
  - 実行時間と速度比は PC によって変わる。結果がテキストと違っても問題ない
  - 並列処理での乱数（`seed = TRUE`）
  - 終わったら `plan(sequential)` に戻す
  - 初回はワーカーの起動に時間がかかる。授業中は待ち時間を見込む

---

## I-1.4 データベースと大規模データ処理

### I-1.4.1 dbplyrによるデータベース操作

- 要点
  - dplyr の処理が SQL に変換される。`show_query()` で確認し、`collect()` で R に取り込む
  - `collect()` はできるだけ最後に（先にデータベース側で絞り込む）
- 実演
  - **QMD にはコードチャンクがない**（本文の説明のみ）。実演するなら、SQLite の一時 DB で `copy_to()` → `filter()` → `show_query()` → `collect()` を別途用意する
- つまずき
  - 遅延評価のため、`collect()` するまで結果が表示されないことがある

### I-1.4.2 ArrowによるParquetデータ処理

- 要点
  - 列指向形式。Parquet の読み書き、他言語とのデータ交換
- 実演
  - 20 銘柄 × 365 日のデータを作り、`arrow::write_parquet()` → `read_parquet()`
- つまずき
  - 保存先は `tempdir()`。セッションを終えると消える

### I-1.4.3 Arrow Dataset

- 要点
  - 複数ファイルを 1 つの Dataset として扱う。銘柄でパーティションを切る
  - `filter` / `select` してから `collect()`
- 実演
  - `write_dataset(..., partitioning = "chr_symbol")` → `open_dataset()` → 絞り込み → `collect()`
- つまずき
  - `collect()` するまでは tibble ではない（表示が違う）

### I-1.4.4 Sparkとの関係

- 要点（説明のみ、コードなし）
  - Arrow は 1 台の PC での効率化、Spark はクラスタで分散する基盤
  - 本書では Arrow と dbplyr を実装対象とし、Spark は位置づけの説明にとどめる

---

## I-1.5 総合演習

### I-1.5.1 分析用データを作る

- 要点：縦持ちにして、グロスリターン（1 + r）と方向（正・負・ゼロ）を付け、`arrange(firm, date)` で並べる
- 実演：`pivot_longer()` → `mutate(case_when(...))`

### I-1.5.2 企業別の要約統計量を計算する

- 要点：`group_by(firm) |> summarise(...)`。`positive_ratio = mean(return > 0)`
- つまずき：論理値の平均が割合になること

### I-1.5.3 累積リターンを計算する

- 要点：`cumprod(gross_return) - 1`
- つまずき
  - 単純リターンの累積は積（和ではない）
  - `cumprod` の前に日付順へ並べておく必要がある
  - 最後の `ungroup()`

### I-1.5.4 purrrで企業別の統計量を再計算する

- 要点：同じ集計を `map_dfr()` で書き、I-1.5.2 の結果と比べる。VaR 的な 5% 分位点も出す

### I-1.5.5 累積リターンを可視化する

- 要点：`geom_line()` に `geom_hline(yintercept = 0)` を重ねる

### まとめ（I-1.5 末尾）

- snake_case / ベクトル化を優先 / 反復は `map()` 系 / dplyr・stringr・tidyr の役割分担 / `|>` に統一 / ggplot は層で組み立てる / パスはプロジェクトルートを基準にする

---

## I-1.6.1 金融データ分析パイプライン

- 要点
  - Parquet・DB → dbplyr / Arrow で絞り込み → `collect()` → 整形 → `nest()` → `map()` → `future_map()` → 集計・モデル・可視化
  - 次章以降はこの流れで財務データと株式データを分析すると予告する
- 注意：見出しが `I-1.6.1` だけで、親の `I-1.6` 見出しがない（目次の表示に影響する）
