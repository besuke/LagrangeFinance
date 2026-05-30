#!/usr/bin/env bash
set -e

echo "=== Create eval chapters ==="

mkdir -p chapters_eval
cp chapters_clean/ch03.qmd chapters_eval/ch03.qmd
cp chapters_clean/ch04.qmd chapters_eval/ch04.qmd
cp chapters_clean/ch05.qmd chapters_eval/ch05.qmd
cp chapters_clean/ch06.qmd chapters_eval/ch06.qmd
cp chapters_clean/ch07.qmd chapters_eval/ch07.qmd
cp chapters_clean/ch08.qmd chapters_eval/ch08.qmd

echo "=== Turn eval on ==="

perl -pi -e 's/#\| eval: false/#| eval: true/g' chapters_eval/*.qmd

echo "=== Patch known problematic IRR chunk ==="

perl -0pi -e 's/(```\{r\}\n#\| eval: true\n#\| message: false\n#\| warning: false\nCF <- c$begin:math:text$\-100\, 230\, \-132$end:math:text$.*?calculate_IRR$begin:math:text$CF$end:math:text$.*?```)/```{r}\n#| eval: false\n#| message: false\n#| warning: false\n# IRRが一意に定まらない例なので、ここは表示のみ\nCF <- c(-100, 230, -132)\ncalculate_IRR(CF)\n```/s' chapters_eval/ch03.qmd

echo "=== Write _quarto.yml ==="

cat > _quarto.yml <<'YAML'
project:
  type: book

book:
  title: "LagrangeFinance"
  subtitle: "Empirical Accounting and Finance with R"
  author: "besuke"
  chapters:
    - index.qmd
    - chapters_eval/ch03.qmd
    - chapters_eval/ch04.qmd
    - chapters_eval/ch05.qmd
    - chapters_eval/ch06.qmd
    - chapters_eval/ch07.qmd
    - chapters_eval/ch08.qmd

format:
  html:
    theme: cosmo
    toc: true
    code-fold: false
    number-sections: false

execute:
  warning: false
  message: false
YAML

echo "=== Render ==="

quarto render

echo "=== Open book ==="

open _book/index.html

echo "=== Done ==="
