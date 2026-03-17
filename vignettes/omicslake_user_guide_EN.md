# OmicsLake User Guide (Markdown Edition)

This Markdown guide is a lightweight navigation document.
For full runnable tutorials, use the R Markdown vignettes listed below.

## Recommended reading order

1. `vignettes/lake_quickstart_EN.Rmd`
2. `vignettes/omicslake_practical_workflow_EN.Rmd`
3. `vignettes/omicslake_layer_use_cases_EN.Rmd`
4. `vignettes/omicslake_comprehensive_guide_EN.Rmd`

## Fastest end-to-end demo

Run:

```bash
bash tools/run_demo_count_edger_limma_voom_ora.sh
```

This executes a canonical pipeline:

- count matrix generation
- edgeR filtering/normalization
- limma-voom differential analysis
- ORA (`limma::goana`)
- lineage/snapshot recording with OmicsLake

## Minimal API starter

```r
library(OmicsLake)
lake <- Lake$new("atlas")
lake$put("counts", counts_df)
out <- lake$get("counts")
lake$snap("baseline")
lake$tree("counts")
```

## Existing pipeline integration

```r
track_script("analysis_pipeline.R", project = "atlas")
```

## Reproducibility strict mode

```r
ol_enable_strict_repro_mode(path = getwd())
```

## Support docs

- `docs/START_HERE.md`
- `docs/FIRST_RUN_CHECKLIST.md`
- `docs/WORKFLOW_RECIPES.md`
- `docs/TROUBLESHOOTING.md`
- `docs/FAQ.md`
