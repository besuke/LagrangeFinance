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

function Test-ProtectedPath([string]$Path) {

  foreach ($protected in $script:protectedDirectories) {
    if (
      $Path.StartsWith(
        $protected,
        [System.StringComparison]::OrdinalIgnoreCase
      )
    ) {
      return $true
    }
  }

  return $false
}


# ============================================================
# 0. Validate / normalize PDF profiles
# ============================================================

Write-Step "0. Fix and validate PDF profiles"

$excludeProfile = Join-Path $ProjectRoot "_quarto-pdf-exclude.yml"
$normalProfile  = Join-Path $ProjectRoot "_quarto-pdf.yml"
$utf8NoBom      = [System.Text.UTF8Encoding]::new($false)

if (-not (Test-Path -LiteralPath $excludeProfile)) {
  throw "Missing profile: $excludeProfile"
}

if (-not (Test-Path -LiteralPath $normalProfile)) {
  throw "Missing profile: $normalProfile"
}


# ----- Exclude PDF profile -----

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


# ----- Normal PDF profile -----

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


# ============================================================
# 1. HTML
# ============================================================

Write-Step "1. Build HTML"

Remove-Item `
  (Join-Path $ProjectRoot "_book") `
  -Recurse `
  -Force `
  -ErrorAction SilentlyContinue

quarto render --to html

Assert-LastExitCode "HTML build failed."


# ============================================================
# 2. Normal PDF
# ============================================================

Write-Step "2. Build Normal PDF"

Remove-Item `
  (Join-Path $ProjectRoot "_pdf_build") `
  -Recurse `
  -Force `
  -ErrorAction SilentlyContinue

quarto render --profile pdf --to pdf

Assert-LastExitCode "Normal PDF build failed."


# ============================================================
# 3. Exclude PDF
# ============================================================

Write-Step "3. Build Exclude PDF"

Remove-Item `
  (Join-Path $ProjectRoot "_pdf_exclude_build") `
  -Recurse `
  -Force `
  -ErrorAction SilentlyContinue

quarto render --profile pdf-exclude --to pdf

Assert-LastExitCode "Exclude PDF build failed."


# ============================================================
# 4. Validate outputs
# ============================================================

Write-Step "4. Validate outputs"

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
}


# ============================================================
# 5. Clean intermediate files
# ============================================================

Write-Step "5. Clean intermediate files"

$script:protectedDirectories = @(
  (Join-Path $ProjectRoot "_book"),
  (Join-Path $ProjectRoot "_pdf_build"),
  (Join-Path $ProjectRoot "_pdf_exclude_build"),
  (Join-Path $ProjectRoot ".git")
)


# ----- Remove *_files directories -----

$intermediateDirs = Get-ChildItem `
  -Path $ProjectRoot `
  -Directory `
  -Recurse `
  -Force `
  -ErrorAction SilentlyContinue |
Where-Object {

  $_.Name -like "*_files" -and
  -not (Test-ProtectedPath $_.FullName)

}

foreach ($dir in $intermediateDirs) {

  Write-Host "Remove: $($dir.FullName)" -ForegroundColor DarkGray

  Remove-Item `
    -LiteralPath $dir.FullName `
    -Recurse `
    -Force `
    -ErrorAction SilentlyContinue
}


# ----- Remove temporary Markdown files -----

$tempPatterns = @(
  "*.knit.md",
  "*.utf8.md"
)

foreach ($pattern in $tempPatterns) {

  $tempFiles = Get-ChildItem `
    -Path $ProjectRoot `
    -Filter $pattern `
    -File `
    -Recurse `
    -Force `
    -ErrorAction SilentlyContinue |
  Where-Object {
    -not (Test-ProtectedPath $_.FullName)
  }

  foreach ($tempFile in $tempFiles) {

    Write-Host "Remove: $($tempFile.FullName)" -ForegroundColor DarkGray

    Remove-Item `
      -LiteralPath $tempFile.FullName `
      -Force `
      -ErrorAction SilentlyContinue
  }
}

Write-Host "`n=== CLEANUP COMPLETE ===" -ForegroundColor Green


# ============================================================
# 6. Commit and push to GitHub
# ============================================================

Write-Step "6. Commit and push to GitHub"

$currentBranch = git branch --show-current
Assert-LastExitCode "Unable to determine current Git branch."

if ($currentBranch -ne "main") {
  throw "Current Git branch is '$currentBranch'. Expected 'main'."
}


$gitStatus = git status --porcelain
Assert-LastExitCode "git status failed."

if ($gitStatus) {

  Write-Host "`nChanges detected:" -ForegroundColor Yellow
  git status --short
  Assert-LastExitCode "git status failed."

  Write-Host "`nAdding changes..." -ForegroundColor Cyan

  git add -A
  Assert-LastExitCode "git add failed."

  $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm"
  $commitMessage = "Build LagrangeFinance $timestamp"

  Write-Host "`nCommit: $commitMessage" -ForegroundColor Cyan

  git commit -m $commitMessage
  Assert-LastExitCode "git commit failed."

  Write-Host "`nPushing to origin/main..." -ForegroundColor Cyan

  git push origin main
  Assert-LastExitCode "git push failed."

  Write-Host "`nGitHub update completed." -ForegroundColor Green

}
else {

  Write-Host "No Git changes detected." -ForegroundColor Green
}


# ============================================================
# 7. Open final outputs
# ============================================================

if ($Open) {

  Write-Step "7. Open final outputs"

  foreach ($file in $outputs) {
    Start-Process $file
  }
}


# ============================================================
# Complete
# ============================================================

Write-Host "`n=== BUILD COMPLETE ===" -ForegroundColor Green

Write-Host "OK: $($outputs[0])" -ForegroundColor Green
Write-Host "OK: $($outputs[1])" -ForegroundColor Green
Write-Host "OK: $($outputs[2])" -ForegroundColor Green

Write-Host "`nAll outputs were generated successfully." -ForegroundColor Green
Write-Host "Intermediate files were cleaned." -ForegroundColor Green
Write-Host "GitHub main is up to date." -ForegroundColor Green