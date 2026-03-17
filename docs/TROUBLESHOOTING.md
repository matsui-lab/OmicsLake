# Troubleshooting

## Installation issues

### `there is no package called 'OmicsLake'`

Install from r-universe:

```r
install.packages(
  "OmicsLake",
  repos = c(
    matsui_lab = "https://matsui-lab.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
```

### Dependency installation fails

Try manual installation:

```r
install.packages(c("DBI", "duckdb", "arrow", "dplyr", "jsonlite", "R6", "rlang"))
```

If Bioconductor packages are missing:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c("edgeR", "limma", "AnnotationDbi", "org.Hs.eg.db"))
```

## Runtime issues

### Cannot write project files

Check write permissions for your working directory and `options("ol.root")`.

### `project not initialized` errors

Run:

```r
library(OmicsLake)
use_lake("my_project")
```

or initialize with:

```r
lake <- Lake$new("my_project")
```

### `track_pipeline()` unavailable

Use a current build and verify package load:

```r
library(OmicsLake)
exists("track_pipeline", where = asNamespace("OmicsLake"), inherits = FALSE)
```

## Vignette code does not execute

Vignettes default to non-evaluated mode for build stability.
To run all chunks yourself:

```r
options(omicslake.vignette.eval = TRUE)
```

Then render the target vignette.

## Reproducibility checks fail

If strict mode is enabled, dirty Git state may block commit/snapshot.
Either commit/stash changes or adjust options intentionally.

```r
options(ol.repro.require_clean_git = FALSE)
```

## Need a known-good end-to-end test

Run:

```bash
bash tools/first_run_check.sh
bash tools/run_demo_count_edger_limma_voom_ora.sh
```

If this succeeds, core dependencies and lineage tracking are functioning.
