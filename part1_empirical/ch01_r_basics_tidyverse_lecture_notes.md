# 講師用メモ：第I-1章 Rによるデータ分析の基礎

元ファイル：`ch01_r_basics_tidyverse.qmd`。括弧内は節番号とチャンクのラベル。

講義の主軸は次の 2 本。章の各節は、この 2 本を説明するための材料として使う。

1. 関数型プログラミング → 副作用の抑制 → 並列処理
2. rlang による式の操作 → データ分析の DSL としての dplyr

---

## 1. 関数型プログラミング → 副作用の抑制 → 並列処理

### 話すこと

- 処理を「入力を受け取り、出力を返す関数」として書く。
  - グローバル変数を書き換えない。共有状態に書き込まない。
  - 結果は戻り値で受け取る。
- そうすると、各要素の処理が互いに独立する。
  - 独立していれば、実行の順番を問わない。
  - 実行する場所（同じセッションか、別のプロセスか）も問わない。
- その結果、`map()` を `future_map()` に置き換えるだけで並列化できる。これが tidyverse（purrr）流に書く実利。
- ただし、副作用を減らしただけで自動的に並列になるわけではない。
  - 並列にするかどうかと、どう並列にするかは、`future` の実行方式（plan）で決まる。
  - Windows の `multisession` は、別の R プロセスに処理を配る。マルチスレッドではない。
  - 副作用の抑制は、並列化の「前提条件を満たすこと」であって、「並列化そのもの」ではない。

### QMD のコードとの結び付け（この順に見せる）

1. **純粋な関数**（I-1.1.4 `custom-function`、`vectorized-function`）
   - `calculate_present_value()` は、引数だけから結果が決まり、外部を変更しない。
   - ベクトルをそのまま受け取れる（ベクトル化）。反復を書く前に、まずこれを検討する。
2. **map は「関数を値に当てる」**（I-1.3.1 `purrr-map`、`purrr-map-dbl`、`purrr-pmap`）
   - ループ変数も、途中で書き換えるオブジェクトもない。結果はベクトルやリストとして返る。
   - `pmap_dbl()` は、表の各行を関数の引数に対応させる。
3. **副作用は walk に隔離する**（I-1.3.1 `purrr-walk`）
   - 表示やファイル出力のように戻り値が要らない処理は、`walk()` に分けて書く。
   - 「計算は map、副作用は walk」と言葉で区別させる。
4. **グループごとの独立した処理**（I-1.3.2 `nest-map-summary`）
   - `nest()` でグループごとのデータを 1 行にまとめ、`map()` で同じ関数を当てる。
   - 各グループは互いに独立しているので、そのまま並列化の単位になる。
5. **map から future_map へ**（I-1.3.3 `compute-pi-path` → `pi-map-sequential` → `pi-future-map-parallel`）
   - `compute_pi_path()` は、試行番号を受け取って tibble を返す関数。
   - `map(tbl_path_ids, compute_pi_chunk)` を `future_map(...)` に替えるだけで、コードの残りは変わらない。
   - `plan(sequential)` と `plan(multisession, workers = ...)` を切り替えるのは実行方式で、計算のコードとは別に指定する。

### 補足として話すこと

- **別プロセスであることの帰結**
  - ワーカーが行った代入（`<<-` やグローバルなリストへの追記）は、親のセッションに戻ってこない。
  - 例として「map の中でグローバルなリストに結果を足していくコード」を見せ、future_map では結果が空になることを示すと伝わりやすい（QMD の外のデモ）。
  - 関数や必要なデータはワーカーへコピーされる。起動とデータ転送のコストがあるので、軽い処理では逐次処理のほうが速い（`pi-benchmark-summary` の速度比で確認する）。
- **乱数は隠れた共有状態**
  - `compute_pi_path()` は、試行番号ごとにシードを設定している。どのワーカーで実行しても結果が再現できるのはこのため。
  - `furrr_options(seed = TRUE)` は、並列処理で乱数の流れを安全に分けるための指定。
- **チャンク化**（`build-pi-chunks`）
  - 1000 回の試行を、ワーカー数ぶんのまとまりに分けてから渡している。
  - 細かく配りすぎると、転送のコストが計算を上回る。
- **用語の区別**
  - `multisession`：別プロセス（Windows を含む）。`multicore`：fork（Windows では使えない）。
  - どちらも R のコードがマルチスレッドで動くわけではない。

### つまずきやすい点

- 「関数型で書けば速くなる」「勝手に並列になる」という誤解。
- map の中で外部の変数を書き換えるコード。逐次処理では動くが、並列にすると壊れる。
- `plan()` を戻し忘れる（QMD では最後に `plan(sequential)` に戻している）。
- 実行時間は PC によって変わる。テキストと速度比が違っても問題ない。

---

## 2. rlang による式の操作 → データ分析の DSL としての dplyr

### 話すこと

- `filter(firm1 > 0)` の `firm1 > 0` は、呼び出した時点では評価されない。
  - R は引数を遅延評価するので、関数は「式そのもの」を受け取れる。
  - dplyr はその式を、データの列を変数とみなす環境（data mask）で評価する。
- rlang は、式・環境・quosure（式とそれを書いた環境の組）を扱うための道具。
  - tidy evaluation は、その上に作られた「列名や分析式をそのまま書ける記法」の仕組み。
- だから dplyr は、データ分析のための小さな言語（DSL）として見られる。
  - 動詞は `select`・`filter`・`mutate`・`summarise`・`group_by`。
  - 列名をそのまま書け、式で計算を表す。
- rlang は「静的な型付けをする仕組み」ではない。
  - 言語を組み立てる仕組み、つまりメタプログラミングの道具と説明する。

### QMD のコードとの結び付け

1. **列名をそのまま書く**（I-1.2.2 `dplyr-filter`、`dplyr-mutate`）
   - `filter(firm1 > 0, firm2 > 0)`、`mutate(spread = firm1 - firm2)`
   - `firm1` という R の変数は存在しない。data mask の中で列として解決される。
2. **式の中の小さな言語**（I-1.1.4 `case-when-introduction`、I-1.2.2 `dplyr-mutate`）
   - `case_when(score >= 80 ~ "A", ...)` の `~` は、「条件 ~ 値」を表す式。
3. **文脈で意味が変わる**（I-1.2.4 `pivot-longer`）
   - `pivot_longer(cols = c(firm1, firm2), ...)` の `c()` はベクトルを作るのではなく、列を選ぶ記法（tidyselect）。
   - 同じ `c()` でも、置かれた文脈で解釈が変わる。DSL らしさが最も見える例。
4. **集計の文法**（I-1.2.2 `dplyr-group-by`、I-1.5.2 `integrated-analysis-summary`）
   - `group_by(firm) |> summarise(mean_return = mean(return), positive_ratio = mean(return > 0))`
   - 列名 `return` は R の関数名と同じだが、data mask の中では列が優先される。
5. **同じ式を別のエンジンで実行する**（I-1.4.3 `arrow-query-dataset`、I-1.4.1 の説明）
   - Arrow の Dataset に対しても、同じ `filter()` / `select()` を書ける。式は Arrow の処理に翻訳され、`collect()` で初めて R に取り込まれる。
   - dbplyr では、同じ式が SQL に翻訳される（`show_query()`）。
   - 式をデータとして扱えるから、書き方（DSL）と実行するエンジンを分けられる。
6. **ggplot2 も同じ考え方**（I-1.2.5 `ggplot-line`）
   - `aes(x = date, y = return, linetype = firm)` は、列名を式として受け取っている。

### 補足デモ（QMD の外、短く）

```r
expr(firm1 > 0)              # 評価されない式
q <- quo(firm1 > 0)          # 式＋環境（quosure）
filter(tbl_daily_stock_return, !!q)

summarise_by <- function(data, group_col, value_col) {
  data |>
    group_by({{ group_col }}) |>
    summarise(mean = mean({{ value_col }}), .groups = "drop")
}
summarise_by(analysis_data, firm, return)   # 自分で DSL の一部を作る
```

- `{{ }}`（embrace）を使うと、列名をそのまま受け取る関数を自作できる。
- 曖昧さを解消したいときは、`.data$列名`（列）と `.env$変数名`（環境の変数）で明示する。

### つまずきやすい点

- 列名を文字列で渡すか、記号（bare name）で渡すかの違い。関数を自作したところで初めて問題になる。
- 列と同じ名前の変数が環境にあるとき、どちらが使われるか（列が優先される）。
- 「rlang = 型チェック」「静的な型付け」という誤解。

---

## 2 本の接点（締めで話す）

- どちらも、「処理を値として扱う」発想。
  - 関数（1 本目）は、計算を値として map に渡す。
  - 式（2 本目）は、分析の意図を値として dplyr やエンジンに渡す。
- その結果、何を計算するか（関数・式）と、どう実行するかを分けられる。
  - どう実行するか：逐次か並列か（`future` の plan）、R か Arrow か DB か（エンジン）。
- I-1.6.1 のパイプライン図（dbplyr / Arrow → `collect` → `nest` → `map` → `future_map`）で全体をつなぐ。
- 注意：I-1.6.1 には親の見出し I-1.6 がない。I-1.4.1（dbplyr）には実行するコードがない（説明だけ）。
