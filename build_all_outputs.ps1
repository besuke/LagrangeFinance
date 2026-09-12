param(
  [switch]$Open
)

$ErrorActionPreference = "Stop"
$ProjectRoot = "C:\AnalyticFin\Projects\LagrangeFinance"

Set-Location $ProjectRoot

function Write-Step([string]$Message) {
  Write-Host "`n=== $Message ===" -ForegroundColor Cyan
}

function Assert-LastExitCode([string]$Message) {
  if ($LASTEXITCODE -ne 0) {
    throw $Message
  }
}

Write-Step "0. Fix and validate PDF profiles"

$excludeProfile = Join-Path $ProjectRoot "_quarto-pdf-exclude.yml"
$normalProfile  = Join-Path $ProjectRoot "_quarto-pdf.yml"
$utf8NoBom = [System.Text.UTF8Encoding]::new($false)

if (-not (Test-Path -LiteralPath $excludeProfile)) {
  throw "Missing profile: $excludeProfile"
}

if (-not (Test-Path -LiteralPath $normalProfile)) {
  throw "Missing profile: $normalProfile"
}

$excludeText = [System.IO.File]::ReadAllText(
  $excludeProfile,
  [System.Text.Encoding]::UTF8
)

$excludeText = $excludeText -replace '^\s+project:', 'project:'

[System.IO.File]::WriteAllText(
  $excludeProfile,
  $excludeText,
  $utf8NoBom
)

$normalText = [System.IO.File]::ReadAllText(
  $normalProfile,
  [System.Text.Encoding]::UTF8
)

if ($normalText -notmatch '(?m)^project:\s*$') {
  $prefix = @"
project:
  output-dir: _pdf_build

book:
  output-file: "lagrange-finance"

"@
  $normalText = $prefix + $normalText

  [System.IO.File]::WriteAllText(
    $normalProfile,
    $normalText,
    $utf8NoBom
  )
}

Write-Step "1. Build HTML"
Remove-Item (Join-Path $ProjectRoot "_book") -Recurse -Force -ErrorAction SilentlyContinue
quarto render --to html
Assert-LastExitCode "HTML build failed."

Write-Step "2. Build Normal PDF"
Remove-Item (Join-Path $ProjectRoot "_pdf_build") -Recurse -Force -ErrorAction SilentlyContinue
quarto render --profile pdf --to pdf
Assert-LastExitCode "Normal PDF build failed."

Write-Step "3. Build Exclude PDF"
Remove-Item (Join-Path $ProjectRoot "_pdf_exclude_build") -Recurse -Force -ErrorAction SilentlyContinue
quarto render --profile pdf-exclude --to pdf
Assert-LastExitCode "Exclude PDF build failed."

Write-Host "`n=== BUILD COMPLETE ===" -ForegroundColor Green

$outputs = @(
  (Join-Path $ProjectRoot "_book\index.html"),
  (Join-Path $ProjectRoot "_pdf_build\lagrange-finance.pdf"),
  (Join-Path $ProjectRoot "_pdf_exclude_build\lagrange-finance-exclude.pdf")
)

foreach ($file in $outputs) {
  if (-not (Test-Path -LiteralPath $file)) {
    throw "Expected output not found: $file"
  }

  Write-Host "OK: $file" -ForegroundColor Green

  if ($Open) {
    Start-Process $file
  }
}

Write-Host "`nAll outputs were generated successfully." -ForegroundColor Green
