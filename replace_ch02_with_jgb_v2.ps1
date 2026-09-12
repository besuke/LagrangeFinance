param(
  [string]$ProjectRoot = "C:\AnalyticFin\Projects\LagrangeFinance"
)

$ErrorActionPreference = "Stop"

$target = Join-Path $ProjectRoot "part2_quantive_riemann\ch02_interest_rate_curves.qmd"
$replacementFile = Join-Path $ProjectRoot "ch02_jgb_replacement.qmd"

if (-not (Test-Path -LiteralPath $target)) {
  throw "Target file not found: $target"
}

if (-not (Test-Path -LiteralPath $replacementFile)) {
  throw "Replacement file not found: $replacementFile"
}

$text = [System.IO.File]::ReadAllText($target, [System.Text.Encoding]::UTF8)
$replacement = [System.IO.File]::ReadAllText($replacementFile, [System.Text.Encoding]::UTF8)

$oldTitle = 'title: "第II-2章 金利カーブ構築と債券先物の最割安銘柄・BPV"'
$newTitle = 'title: "第II-2章 金利カーブ構築と日本国債先物 ― Basis・IRR・CTD"'

$text = $text.Replace($oldTitle, $newTitle)

$marker = '## II-2.5 国債先物の基礎'
$idx = $text.IndexOf($marker)

if ($idx -lt 0) {
  throw "Section marker not found: $marker"
}

$newText = $text.Substring(0, $idx) + $replacement

[System.IO.File]::WriteAllText(
  $target,
  $newText,
  [System.Text.UTF8Encoding]::new($false)
)

Write-Host "Updated: $target"
Write-Host "Replaced II-2.5 onward with JGB futures section."
