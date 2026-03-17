# Start Here

This page is the fastest onboarding route for first-time OmicsLake users.

## 1. Install

```r
install.packages(
  "OmicsLake",
  repos = c(
    matsui_lab = "https://matsui-lab.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
```

## 2. Confirm your environment works

```bash
bash tools/first_run_check.sh
```

or run directly in R:

```r
library(OmicsLake)
lake <- Lake$new("start_here_check")
lake$put("hello", data.frame(x = 1:3))
print(lake$get("hello"))
lake$tree("hello")
```

Expected outcome:

- no installation error
- a small data frame is printed
- lineage query executes

## 3. Pick one learning path

- Minimal basics: `vignettes/lake_quickstart_EN.Rmd`
- Practical analysis flow: `vignettes/omicslake_practical_workflow_EN.Rmd`
- Layer-by-layer representative use cases: `vignettes/omicslake_layer_use_cases_EN.Rmd`
- Full feature reference: `vignettes/omicslake_comprehensive_guide_EN.Rmd`

## 4. Run canonical bioinformatics demo

```bash
bash tools/run_demo_count_edger_limma_voom_ora.sh
```

This runs:

1. count matrix creation
2. edgeR filtering + normalization
3. limma-voom differential analysis
4. ORA with `limma::goana`
5. lineage/snapshot recording in OmicsLake

## 5. Integrate with your existing script

```r
library(OmicsLake)
use_lake("my_project")

track_script("analysis_pipeline.R", project = "my_project")
```

## Next documents

- First run checklist: `docs/FIRST_RUN_CHECKLIST.md`
- User journeys: `docs/USER_JOURNEYS.md`
- Workflow recipes: `docs/WORKFLOW_RECIPES.md`
- Troubleshooting: `docs/TROUBLESHOOTING.md`
- FAQ: `docs/FAQ.md`

Validation command:

```bash
Rscript tools/check_layer_use_cases_vignettes.R
```
