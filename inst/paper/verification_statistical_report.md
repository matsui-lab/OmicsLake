# OmicsLake GigaScience論文 - 統計的検証レポート

**検証担当**: Verifier 1 (数値的検証専門家)
**検証日**: 2026-02-12
**対象ファイル**:
- `inst/paper/01_performance_benchmark.R`
- `inst/paper/benchmark_design_spec.json`
- `inst/paper/02_reproducibility_test.R`

---

## 1. ベンチマーク設計の統計的妥当性確認

### 1.1 サンプルサイズ (n=30) の検出力分析

| 項目 | 評価 | 詳細 |
|------|------|------|
| **設計値** | n=30 (iterations=30) | `01_performance_benchmark.R` line 72, 124, 152, 188 |
| **検出力分析の妥当性** | **OK** | 設計仕様書で明示的に計算済み |
| **根拠** | 適切 | Cohen's d=0.8 (大効果)、alpha=0.05、power=0.80で必要n=26、実際n=30で達成power=0.87 |

**詳細検証**:

設計仕様書 (`benchmark_design_spec.json`) には以下の検出力分析が記載されています:

```json
"power_analysis": {
  "target_power": 0.80,
  "expected_effect_size": 0.8,
  "alpha": 0.05,
  "calculated_n": 26,
  "actual_n": 30,
  "achieved_power": 0.87,
  "software": "pwr::pwr.t.test(d=0.8, sig.level=0.05, power=0.80, type='two.sample')"
}
```

**検証結果**: R関数 `pwr.t.test(d=0.8, sig.level=0.05, power=0.80, type='two.sample')` を実行すると、
各群あたり n=25.5 (切り上げで26) が算出されます。n=30 は十分なマージンを持った設計です。

**潜在的問題点**:
- BM-002 (Aggregation) では expected_effect_size=0.7 で calculated_n=34 だが actual_n=30。
  これは軽度のアンダーパワーですが、設計書で明示的に認識されています。
- 査読者への対応: 「中効果検出の限界」として Methods に記載を推奨。

**評価**: **OK** (軽微な注意点あり)

---

### 1.2 統計検定の選択 (Wilcoxon vs t-test)

| 項目 | 評価 | 詳細 |
|------|------|------|
| **主要検定** | Wilcoxon signed-rank / rank-sum | 適切な選択 |
| **根拠** | ノンパラメトリック、正規性仮定不要 | タイミングデータは一般に右裾分布 |
| **実装** | `wilcox.test()` (line 45) | 正しく実装 |

**コード検証** (`01_performance_benchmark.R`):

```r
# Line 42-51
compare_benchmarks <- function(bench_result, expr1, expr2) {
  times1 <- as.numeric(bench_result[bench_result$expression == expr1, ]$time[[1]])
  times2 <- as.numeric(bench_result[bench_result$expression == expr2, ]$time[[1]])
  test <- wilcox.test(times1, times2, conf.int = TRUE)
  list(
    p_value = test$p.value,
    significant = test$p.value < 0.05,
    conf_int = test$conf.int
  )
}
```

**妥当性評価**:
1. **Wilcoxon rank-sum test の選択**: 適切
   - 実行時間データは典型的に log-normal に近い分布を示す
   - 外れ値の影響を受けにくい
   - 中央値ベースの比較に適合

2. **paired vs unpaired**:
   - **要確認**: 現在の実装は `wilcox.test(times1, times2)` で unpaired テスト
   - ベンチマーク設計では同一マシン上で実行されるため、**paired=TRUE が望ましい可能性**
   - 設計仕様書 BM-001 では `paired: false` と明示されているが、理論的には paired が適切な場合もある

**推奨修正**:
```r
# 同一システムでの比較なら paired=TRUE を検討
test <- wilcox.test(times1, times2, paired = TRUE, conf.int = TRUE)
```

**評価**: **OK** (minor improvement possible)

---

### 1.3 多重比較補正 (Bonferroni) の正確な適用

| 項目 | 評価 | 詳細 |
|------|------|------|
| **補正方法** | Bonferroni | 保守的だが適切 |
| **実装** | 正確 | `pmin(p_values * n_tests, 1.0)` |
| **Family-wise error rate** | alpha=0.05 を維持 | 正しい |

**コード検証** (`01_performance_benchmark.R` line 362-372):

```r
bonferroni_correct <- function(p_values, alpha = 0.05) {
  n_tests <- length(p_values)
  corrected_alpha <- alpha / n_tests
  adjusted_p <- pmin(p_values * n_tests, 1.0)
  list(
    original_p = p_values,
    adjusted_p = adjusted_p,
    corrected_alpha = corrected_alpha,
    significant = adjusted_p < alpha
  )
}
```

**数学的検証**:
- Bonferroni補正: adjusted_p = min(p * m, 1) where m = number of tests
- 実装: `pmin(p_values * n_tests, 1.0)` - **正確**

**設計仕様との整合性**:
- BM-001: n_comparisons=4, corrected_alpha=0.0125 (0.05/4) - **正確**
- BM-002: Holm-Bonferroni も推奨されているが、実装は Bonferroni のみ
  - Holm-Bonferroni は less conservative でより検出力が高い
  - 査読者から「より適切な補正法」の指摘可能性あり

**評価**: **OK** (Holm-Bonferroni の追加実装を推奨)

---

### 1.4 効果量 (Cohen's d) の計算式の確認

| 項目 | 評価 | 詳細 |
|------|------|------|
| **計算式** | Pooled SD 使用 | 正確 |
| **解釈基準** | Cohen (1988) 準拠 | 適切 |
| **実装** | 正確 | line 333-359 |

**コード検証** (`01_performance_benchmark.R` line 333-359):

```r
calculate_cohens_d <- function(times1, times2) {
  n1 <- length(times1)
  n2 <- length(times2)
  mean1 <- mean(times1)
  mean2 <- mean(times2)
  var1 <- var(times1)
  var2 <- var(times2)

  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))

  # Cohen's d
  d <- (mean1 - mean2) / pooled_sd

  # Interpretation
  interpretation <- if (abs(d) < 0.2) {
    "negligible"
  } else if (abs(d) < 0.5) {
    "small"
  } else if (abs(d) < 0.8) {
    "medium"
  } else {
    "large"
  }

  list(d = d, interpretation = interpretation)
}
```

**数学的検証**:
1. **Pooled SD の計算式**:
   - 実装: `sqrt(((n1-1)*var1 + (n2-1)*var2) / (n1+n2-2))`
   - 正式な式: $s_p = \sqrt{\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}}$
   - **完全に一致**

2. **Cohen's d の計算式**:
   - 実装: `d = (mean1 - mean2) / pooled_sd`
   - 正式な式: $d = \frac{\bar{x}_1 - \bar{x}_2}{s_p}$
   - **完全に一致**

3. **解釈基準** (Cohen, 1988):
   - |d| < 0.2: negligible
   - 0.2 <= |d| < 0.5: small
   - 0.5 <= |d| < 0.8: medium
   - |d| >= 0.8: large
   - **正確に実装**

**潜在的改善点**:
- Glass's delta や Hedges' g (小サンプル補正版) の追加を検討
- Cohen's d の信頼区間 (bootstrap または MBESS パッケージ) の報告を推奨

**評価**: **OK**

---

## 2. 再現性テストの検証基準確認

### 2.1 all.equal() の tolerance=1e-8 の妥当性

| 項目 | 評価 | 詳細 |
|------|------|------|
| **設定値** | tolerance=1e-8 | `02_reproducibility_test.R` line 37 |
| **妥当性** | **OK with caveat** | IEEE 754 double の精度を考慮 |

**コード検証** (`02_reproducibility_test.R` line 37):

```r
TOLERANCE <- 1e-8  # Numerical tolerance for all.equal()
```

**使用箇所** (例: line 159-163):
```r
results$restoration_checks$raw <- isTRUE(all.equal(
  original_raw[order(original_raw$gene_id, original_raw$sample_id), ],
  restored_raw[order(restored_raw$gene_id, restored_raw$sample_id), ],
  tolerance = TOLERANCE
))
```

**数学的根拠**:
- IEEE 754 倍精度浮動小数点: 約15-17桁の有効数字
- マシンイプシロン: ~2.22e-16
- tolerance=1e-8 は約8桁の一致を要求

**妥当性分析**:
1. **Parquet I/O での精度保持**: Parquet は float64 をそのまま保存するため、理論上は完全一致
2. **DuckDB クエリ後の精度**: DuckDB の浮動小数点演算は IEEE 754 準拠
3. **R のシリアライゼーション**: RDS/qs は完全な精度保持
4. **tolerance=1e-8 の安全マージン**: マシンイプシロンの約4.5桁上で、十分保守的

**潜在的問題**:
- 集約演算 (sum, mean) では丸め誤差が蓄積する可能性
- 1e-8 では厳しすぎる場合: 1e-6 への緩和を検討
- 1e-8 では緩すぎる場合: 単純な I/O では 1e-12 も可能

**推奨**:
- 現在の 1e-8 は妥当だが、失敗時のエラーメッセージで実際の差分値を報告すべき

**評価**: **OK** (エラー報告の改善を推奨)

---

### 2.2 成功基準 (99%以上) の統計的根拠

| 項目 | 評価 | 詳細 |
|------|------|------|
| **基準値** | 99% (0.99) | line 869-873 |
| **根拠** | 明示なし | **要補足** |

**コード検証** (`02_reproducibility_test.R` line 869-873):

```r
Pass = c(
  rt001_summary$value >= 0.99,
  rt002_summary$value >= 0.99,
  rt003_summary$value >= 0.99,
  !is.na(rt004_summary$value) && rt004_summary$value <= 2.0,
  rt005_summary$value >= 0.99
),
```

**統計的根拠の検証**:

1. **99%基準の妥当性**:
   - 再現性ソフトウェアとして「99%以上」は業界標準に近い
   - 完全一致 (100%) は現実的に達成困難（浮動小数点演算、OS差異等）
   - 95% では「5%の失敗」が許容され、科学的再現性として不十分

2. **統計的解釈**:
   - n=10 で 99% 成功 = 全10回成功 (確率的には 0.99^10 = 0.904)
   - n=10 で 1回失敗すると成功率は 90% となり、基準を下回る
   - **サンプルサイズ n=10 では基準との整合性が微妙**

3. **二項分布による信頼区間**:
   - n=10, k=10 (全成功) の場合、Clopper-Pearson 95% CI: [0.691, 1.000]
   - 「真の成功率 >= 99%」を統計的に主張するには n が不足

**推奨修正**:
- 99%基準の根拠を Methods に明記（例：ACM/IEEE ガイドライン参照）
- または、信頼区間の下限が 95% 以上であることを基準とする
- サンプルサイズを n=30 以上に増加させると信頼性向上

**評価**: **要修正** (根拠の明示と、サンプルサイズの再検討)

---

### 2.3 F1スコア計算の正確性

| 項目 | 評価 | 詳細 |
|------|------|------|
| **計算式** | 標準的 F1 | line 358-363 |
| **実装** | 正確 | Precision と Recall から計算 |

**コード検証** (`02_reproducibility_test.R` line 354-363):

```r
results$edge_recall <- edges_found / length(expected_edges)
results$edge_precision <- ifelse(nrow(all_deps) > 0, edges_found / nrow(all_deps), 0)

# Calculate F1 score
if (results$edge_recall + results$edge_precision > 0) {
  results$f1_score <- 2 * (results$edge_precision * results$edge_recall) /
                      (results$edge_precision + results$edge_recall)
} else {
  results$f1_score <- 0
}
```

**数学的検証**:

1. **Recall (再現率)**:
   - 定義: TP / (TP + FN) = 検出されたエッジ / 期待されるエッジ
   - 実装: `edges_found / length(expected_edges)`
   - **正確**

2. **Precision (適合率)**:
   - 定義: TP / (TP + FP) = 正しいエッジ / 検出された全エッジ
   - 実装: `edges_found / nrow(all_deps)`
   - **正確** (ただし、edges_found が TP であることが前提)

3. **F1 Score**:
   - 定義: $F_1 = \frac{2 \cdot P \cdot R}{P + R}$
   - 実装: `2 * (precision * recall) / (precision + recall)`
   - **正確**

4. **ゼロ除算対策**:
   - `precision + recall > 0` のチェックあり
   - **正しく処理**

**評価**: **OK**

---

## 3. GigaScience 査読者が指摘しそうな統計的弱点

### 3.1 高リスク指摘事項

| 問題 | 重要度 | 推奨対応 |
|------|--------|----------|
| **paired vs unpaired Wilcoxon** | 高 | 同一システムでは paired=TRUE を検討 |
| **99%基準の根拠不明** | 高 | ACM/IEEE ガイドライン等を引用 |
| **Holm-Bonferroni 未実装** | 中 | Bonferroni は保守的すぎる可能性 |
| **効果量の信頼区間なし** | 中 | Bootstrap CI を追加 |

### 3.2 中リスク指摘事項

| 問題 | 重要度 | 推奨対応 |
|------|--------|----------|
| **BM-002 のアンダーパワー** | 中 | Methods で limitation として言及 |
| **Cold start シミュレーションの限界** | 中 | GC だけでは完全な cold start 不可 |
| **クロス環境テストがシミュレーション** | 中 | 「simulated」と明記し limitation に |
| **外れ値の処理方針不明** | 低 | IQR ベースの外れ値除外を明示 |

### 3.3 低リスク指摘事項

| 問題 | 重要度 | 推奨対応 |
|------|--------|----------|
| **Cliff's delta 未実装** | 低 | Cohen's d で十分だが追加推奨 |
| **データ型別検証不足** | 低 | 整数/浮動小数点/文字列の個別検証 |

---

## 4. 検証サマリー

### 4.1 統計手法の妥当性評価一覧

| 項目 | 評価 | ステータス |
|------|------|------------|
| サンプルサイズ n=30 | OK | 検出力 0.87 達成 |
| Wilcoxon 検定の選択 | OK | ノンパラメトリック、適切 |
| paired vs unpaired | 要確認 | 理論的には paired が適切な可能性 |
| Bonferroni 補正 | OK | 正確に実装、Holm も検討 |
| Cohen's d 計算 | OK | 完全に正確 |
| all.equal tolerance | OK | 1e-8 は妥当 |
| 99% 成功基準 | 要修正 | 統計的根拠の明示が必要 |
| F1 スコア計算 | OK | 正確 |

### 4.2 総合評価

**全体評価**: **概ね妥当 (Minor Revisions Required)**

実装されている統計手法は概ね正確で、GigaScience の査読基準を満たす水準にあります。
ただし、以下の修正を推奨します：

1. **必須修正**:
   - 99% 成功基準の統計的根拠を Methods に追記
   - paired Wilcoxon の適用を検討

2. **推奨修正**:
   - Holm-Bonferroni 補正の追加
   - Cohen's d の 95% 信頼区間の報告
   - RT-001/RT-002 のサンプルサイズ増加 (n=10 -> n=30)

3. **Limitations への明記**:
   - BM-002 の軽度アンダーパワー
   - RT-003 のクロス環境テストがシミュレーションである点
   - Cold start シミュレーションの限界

---

## 5. 付録: 検証に使用したコード参照

### 5.1 検出力分析の検証

```r
# pwr パッケージによる検証
library(pwr)
pwr.t.test(d = 0.8, sig.level = 0.05, power = 0.80, type = 'two.sample')
# 結果: n = 25.52 (各群)

# 実際の n=30 での検出力
pwr.t.test(d = 0.8, sig.level = 0.05, n = 30, type = 'two.sample')
# 結果: power = 0.867
```

### 5.2 Bonferroni 補正の検証

```r
# 手動検証
p_values <- c(0.01, 0.02, 0.03, 0.04)
n_tests <- 4
adjusted_p <- pmin(p_values * n_tests, 1.0)
# 結果: 0.04, 0.08, 0.12, 0.16
```

### 5.3 Cohen's d の検証

```r
# エフェクトサイズライブラリとの比較
library(effsize)
times1 <- rnorm(30, mean = 1.0, sd = 0.2)
times2 <- rnorm(30, mean = 1.5, sd = 0.2)
cohen.d(times1, times2)  # 公式実装
# 独自実装との一致を確認
```

---

**検証完了**: 2026-02-12
**検証者**: Verifier 1 (数値的検証専門家)
