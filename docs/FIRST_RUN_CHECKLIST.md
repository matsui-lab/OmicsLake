# First Run Checklist

Use this checklist before starting real analysis.

## Environment

- R version is `>= 4.5.0`
- write permission exists for your project directory
- internet access is available if dependencies must be installed

## Package availability

Run:

```bash
bash tools/first_run_check.sh
```

or verify manually:

```r
library(OmicsLake)
```

If this fails, reinstall from README instructions.

## Lake basic I/O check

```r
lake <- Lake$new("first_run_check")
lake$put("tiny", data.frame(a = 1:2, b = c("x", "y")))
out <- lake$get("tiny")
stopifnot(nrow(out) == 2)
```

## Lineage check

```r
lake$ref("tiny") |>
  dplyr::mutate(a2 = a * 2) |>
  save_as("tiny2", lake)

lake$tree("tiny2")
```

## Reproducibility baseline

```r
options(
  ol.repro.capture = TRUE,
  ol.repro.path = getwd(),
  ol.repro.require_clean_git = TRUE,
  ol.snapshot.auto_validate = TRUE,
  ol.snapshot.validate.mode = "warn"
)
```

## Optional strict mode

```r
ol_enable_strict_repro_mode(path = getwd())
```

## Pass criteria

- all code runs without error
- object write/read is successful
- lineage tree is produced
- strict mode can be enabled
