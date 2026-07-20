$ErrorActionPreference = "Stop"

$projectRoot = "C:\AnalyticFin\Projects\LagrangeFinance"
Set-Location $projectRoot

Write-Host ""
Write-Host "=== Repository ==="

$repoRoot = (git rev-parse --show-toplevel).Trim()
$branch = (git branch --show-current).Trim()

Write-Host "Root   : $repoRoot"
Write-Host "Branch : $branch"

if ($branch -ne "main") {
  throw "Current branch is not main: $branch"
}

Write-Host ""
Write-Host "=== Remote check ==="

git fetch origin

$behindCount = [int](git rev-list --count HEAD..origin/main)
$aheadCount = [int](git rev-list --count origin/main..HEAD)

Write-Host "Behind origin/main: $behindCount"
Write-Host "Ahead origin/main : $aheadCount"

if ($behindCount -gt 0) {
  throw "Local main is behind origin/main. Stop before changing files."
}

Write-Host ""
Write-Host "=== Status before cleanup ==="

git status --short

Write-Host ""
Write-Host "=== Update .gitignore ==="

$ignorePath = Join-Path $projectRoot ".gitignore"

$ignoreEntries = @(
  "simulation_data/derived/",
  "*.bak",
  "*.bak_*",
  "*.tmp",
  "*.patch",
  "part1_empirical/PV.png",
  "part1_empirical/IRR.pdf"
)

$existingIgnoreLines = @()

if (Test-Path $ignorePath) {
  $existingIgnoreLines = @(Get-Content -Path $ignorePath)
}

$missingIgnoreEntries = @(
  $ignoreEntries |
    Where-Object { $_ -notin $existingIgnoreLines }
)

if ($missingIgnoreEntries.Count -gt 0) {
  Add-Content -Path $ignorePath -Value ""

  $missingIgnoreEntries |
    ForEach-Object {
      Add-Content -Path $ignorePath -Value $_
    }
}

Write-Host ""
Write-Host "=== Remove reproducible cache directories ==="

$cacheDirectories = @(
  ".quarto",
  "_freeze",
  "_book"
)

$cacheDirectories |
  ForEach-Object {
    $target = Join-Path $projectRoot $_

    if (Test-Path $target) {
      Write-Host "Removing directory: $target"
      Remove-Item -Path $target -Recurse -Force
    }
  }

Write-Host ""
Write-Host "=== Remove legacy generated chapter files ==="

$legacyOutputs = @(
  "part1_empirical\PV.png",
  "part1_empirical\IRR.pdf"
)

$legacyOutputs |
  ForEach-Object {
    $target = Join-Path $projectRoot $_

    if (Test-Path $target) {
      Write-Host "Removing file: $target"
      Remove-Item -Path $target -Force
    }
  }

Write-Host ""
Write-Host "=== Remove backup, patch, and temporary files ==="

$temporaryFiles = @(
  Get-ChildItem -Path $projectRoot -Recurse -File -Force |
    Where-Object {
      $_.Name -like "*.bak" -or
      $_.Name -like "*.bak_*" -or
      $_.Extension -eq ".patch" -or
      $_.Extension -eq ".tmp"
    }
)

if ($temporaryFiles.Count -gt 0) {
  $temporaryFiles |
    Sort-Object FullName |
    Select-Object FullName, Length, LastWriteTime |
    Format-Table -AutoSize

  $temporaryFiles |
    Remove-Item -Force
} else {
  Write-Host "No backup, patch, or temporary files found."
}

Write-Host ""
Write-Host "=== Working-tree diff check ==="

git diff --check

Write-Host ""
Write-Host "=== Stage all remaining changes ==="

git add -A

Write-Host ""
Write-Host "=== Staged diff check ==="

git diff --cached --check

Write-Host ""
Write-Host "=== Staged status and statistics ==="

git status --short
git diff --cached --stat

$stagedFiles = @(git diff --cached --name-only)

if ($stagedFiles.Count -eq 0) {
  Write-Host ""
  Write-Host "No staged changes. Nothing to commit."
} else {
  Write-Host ""
  Write-Host "=== Commit ==="

  git commit -m "Update Part I empirical finance chapters"

  Write-Host ""
  Write-Host "=== Push main ==="

  git push origin main
}

Write-Host ""
Write-Host "=== Final repository status ==="

git status -sb

Write-Host ""
Write-Host "=== Remaining untracked files: preview only ==="

git clean -nd

Write-Host ""
Write-Host "The derived CSV directory was kept locally and ignored by Git."
Write-Host "Commit and cleanup completed."
