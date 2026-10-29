関数名・参照先修正版（b04be93対応）

ZIP内のRフォルダを、LagrangeFinanceプロジェクトのRフォルダに上書きしてください。
既存のExcelは変更しません。

関数名：
（2026-10-30追記）OISカーブとASWの計算はGaussQuantへ移管し、
R/instance_swap_curve.R、R/compute_asset_swap_ISO.R、R/compute_asset_swap_basis_JPY.R は削除した。
現在の関数：GaussQuant::ir_build_ois_curve_GQH()、GaussQuant::ois_asset_swap_GQL()、
GaussQuant::ois_asset_swap_jpy_basis_GQL()。以下の記述は移管前の配布時点のもの。

compute_asset_swap_ISO.R → compute_asset_swap_ISO()
compute_asset_swap_basis_JPY.R → compute_asset_swap_basis_JPY()

サンプルと検証コードのsource()、関数内部の呼出先、コメントも修正済み。
旧関数名への参照と、削除済みの旧ファイル名への参照はありません。

実行：
source("R/sample_bond_asset_swap.R")

2022年例：同じファイルの run_historical <- TRUE にして実行。
historical$asw$valuation / historical$asw$cashflows / historical$prices に結果が残ります。

検証：実ExcelとGaussQuantで5通貨Native ASW、4通貨対円ASW、価格テーブル、
ゼロベーシス・符号・為替水準・価格感応度・CCS/ASW NPV照合を実行済み。
2022年例も別途実行して確認。
GitHubへの反映は行っていません。
