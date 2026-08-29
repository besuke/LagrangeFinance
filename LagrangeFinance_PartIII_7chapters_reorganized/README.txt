LagrangeFinance 第III部 7章再編版
================================

第III-1章  金融工学における計算技術の発展と並列コンピューティング
第III-2章  分散コンピューティングとJob / Task Control
第III-3章  エンタープライズ金融計算システムの構築
第III-4章  自動微分とDifferentiable Pricing
第III-5章  Option Greeks：GaussQuant vs Monte Carlo + torch
第III-6章  金利リスク：DV01からCurve Gradientへ
第III-7章  PRDC：多因子Monte Carloと自動微分

配置先:
  part3_enterprise_riemann/

注意:
- すべてのRチャンクについて #| label: を監査済み。
- QMDはUTF-8（BOMなし）で保存。
- _quarto_part3_snippet.yml は _quarto.yml の第III部置換用。
- III-5 は直近の GaussQuant vs Monte Carlo + torch 改訂版を採用。

Chunk label audit
-----------------
ch01_computational_evolution_parallel.qmd: labels=13, missing=none, duplicates=none
ch02_distributed_computing_jobtask.qmd: labels=11, missing=none, duplicates=none
ch03_enterprise_financial_computing.qmd: labels=10, missing=none, duplicates=none
ch04_automatic_differentiation.qmd: labels=4, missing=none, duplicates=none
ch05_option_greeks_autodiff.qmd: labels=17, missing=none, duplicates=none
ch06_interest_rate_risk_autodiff.qmd: labels=20, missing=none, duplicates=none
ch07_prdc_autodiff.qmd: labels=16, missing=none, duplicates=none
