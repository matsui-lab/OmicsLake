# Workflow Recipes

This document provides practical recipes for common use cases.

## Recipe 1: Existing script with minimal changes

```r
library(OmicsLake)
use_lake("project")
track_script("analysis_pipeline.R", project = "project")
```

Use this when you want immediate provenance without refactoring code.

## Recipe 2: Existing code block in-place

```r
library(OmicsLake)
use_lake("project")

track_pipeline({
  counts <- read.csv("counts.csv")
  qc <- subset(counts, total_counts > 10)
  write.csv(qc, "counts_qc.csv", row.names = FALSE)
}, snapshot = "qc_v1", store_observation = TRUE)
```

Use this when your workflow is not yet script modularized.

## Recipe 3: dplyr-native lineage tracking

```r
library(OmicsLake)
library(dplyr)

lake <- Lake$new("project")
lake$put("counts", counts_df)
lake$put("meta", meta_df)

lake$ref("counts") |>
  left_join(lake$ref("meta"), by = "sample_id") |>
  filter(qc_pass) |>
  group_by(condition) |>
  summarize(mean_expr = mean(expression), .groups = "drop") |>
  save_as("summary", lake)
```

Use this when your team already uses dplyr heavily.

## Recipe 4: Canonical RNA-seq demo

```bash
bash tools/run_demo_count_edger_limma_voom_ora.sh
```

Outputs include DEG and ORA CSV files plus recorded lineage metadata.

## Recipe 5: Strict reproducibility defaults

```r
library(OmicsLake)

ol_enable_strict_repro_mode(
  path = getwd(),
  prompt_id = "analysis-run-001"
)
```

Use this when analysis is publication-facing or regulated.

## Recipe 6: Team handoff checkpoint

```r
lake <- Lake$new("project")
lake$snap("handoff_2026_02_22", note = "handoff to collaborator")
lake$log()
```

Use this before sharing intermediate artifacts across teams.
