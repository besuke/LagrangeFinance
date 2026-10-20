# クオンツ問題集：解答・解説

全183問を8本の独立したQuartoファイルに分けました。各問に問題文・解答・解説を収録しています。

|ファイル|対象|問題数|
|---|---|---:|
|[ch01_general.qmd](ch01_general.qmd)|第1章 総合問題|15|
|[ch02_1_mathematics.qmd](ch02_1_mathematics.qmd)|2.1 数学|15|
|[ch02_2_linear_algebra.qmd](ch02_2_linear_algebra.qmd)|2.2 線形代数|10|
|[ch02_3_financial_instruments.qmd](ch02_3_financial_instruments.qmd)|2.3 金融商品|26|
|[ch02_4_cpp.qmd](ch02_4_cpp.qmd)|2.4 C++|34|
|[ch02_5_numerical_methods.qmd](ch02_5_numerical_methods.qmd)|2.5 数値計算|11|
|[ch02_6_stochastic_calculus.qmd](ch02_6_stochastic_calculus.qmd)|2.6 確率解析|33|
|[ch02_7_brainteasers.qmd](ch02_7_brainteasers.qmd)|2.7 頭の体操|39|

## 使い方

単体で `quarto render ch01_general.qmd --to html` のように実行できます。HTMLは数式にMathJaxを用います。コードは自動実行しません。C++例はC++17を基本とする独立した断片で、コンパイラによる実行確認は未実施です。

原問題に条件不足がある設問では、必要な仮定と別解釈を説明しました。これは独自の学習用解答であり、原著の公式解答ではありません。

元ファイル：besuke/LagrangeFinance、docs/quant_questions_ja.qmd。読取コミット：cbce371663451dd595b5ced343b36cae589512a2。
