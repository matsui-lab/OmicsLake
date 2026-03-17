# OmicsLake ユーザーガイド（Markdown版）

このMarkdown版ガイドは、導線を最短化するためのナビゲーション用ドキュメントです。
実行可能な詳細チュートリアルは、以下のR Markdownビネットを参照してください。

## 推奨読了順

1. `vignettes/lake_quickstart.Rmd`
2. `vignettes/omicslake_practical_workflow.Rmd`
3. `vignettes/omicslake_layer_use_cases.Rmd`
4. `vignettes/omicslake_comprehensive_guide.Rmd`

## 最短でのエンドツーエンド実行

以下を実行してください。

```bash
bash tools/run_demo_count_edger_limma_voom_ora.sh
```

このデモは次を一括実行します。

- count行列の生成
- edgeRでのフィルタリング/正規化
- limma-voomによる差次解析
- ORA（`limma::goana`）
- OmicsLakeでのリネージ/スナップショット記録

## 最小APIスターター

```r
library(OmicsLake)
lake <- Lake$new("atlas")
lake$put("counts", counts_df)
out <- lake$get("counts")
lake$snap("baseline")
lake$tree("counts")
```

## 既存パイプラインへの最小導入

```r
track_script("analysis_pipeline.R", project = "atlas")
```

## 再現性の厳格モード

```r
ol_enable_strict_repro_mode(path = getwd())
```

## 補助ドキュメント

- `docs/START_HERE.md`
- `docs/FIRST_RUN_CHECKLIST.md`
- `docs/WORKFLOW_RECIPES.md`
- `docs/TROUBLESHOOTING.md`
- `docs/FAQ.md`
