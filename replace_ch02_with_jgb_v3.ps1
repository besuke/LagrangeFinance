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

$utf8NoBom = New-Object System.Text.UTF8Encoding($false)

$text = [System.IO.File]::ReadAllText($target, [System.Text.Encoding]::UTF8)
$replacement = [System.IO.File]::ReadAllText($replacementFile, [System.Text.Encoding]::UTF8)

# Replace the YAML title without embedding Japanese text directly in this .ps1.
$titleBytes = [System.Convert]::FromBase64String("dGl0bGU6ICLnrKxJSS0y56ugIOmHkeWIqeOCq+ODvOODluani+evieOBqOaXpeacrOWbveWCteWFiOeJqSDigJUgQmFzaXPjg7tJUlLjg7tDVEQi")
$newTitle = [System.Text.Encoding]::UTF8.GetString($titleBytes)
$text = [regex]::Replace(
  $text,
  '(?m)^title:\s*".*II-2.*"$',
  [System.Text.RegularExpressions.MatchEvaluator]{ param($m) $newTitle },
  1
)

# Find the II-2.5 section by an ASCII-only pattern.
$match = [regex]::Match($text, '(?m)^##\s+II-2\.5\s+.*$')
if (-not $match.Success) {
  throw "Section II-2.5 not found."
}

$newText = $text.Substring(0, $match.Index) + $replacement

[System.IO.File]::WriteAllText($target, $newText, $utf8NoBom)

Write-Host "Updated: $target"
Write-Host "Replaced II-2.5 onward with the JGB futures section."
