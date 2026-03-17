# AI Agent Integration Guide

This guide describes how to maintain analysis consistency when code is generated or edited by AI agents.

## Goal

Keep AI-assisted iteration speed while preserving reproducibility and provenance.

## Recommended baseline

```r
library(OmicsLake)

ol_enable_strict_repro_mode(
  path = getwd(),
  prompt_id = "agent-run-001"
)
```

## Track script-level execution

```r
track_script(
  "analysis_pipeline.R",
  project = "analysis_project",
  save_result = TRUE,
  result_name = "analysis_summary"
)
```

## Track ad-hoc generated code blocks

```r
track_pipeline({
  # code generated or modified by agent
  counts <- read.csv("counts.csv")
  counts$log_expr <- log2(counts$expr + 1)
  write.csv(counts, "counts_log.csv", row.names = FALSE)
}, snapshot = "agent_iter_001", store_observation = TRUE)
```

## Metadata policy

Set run metadata in options for every AI-run batch:

```r
options(
  ol.agent.name = "codex",
  ol.agent.prompt_id = "prompt-2026-02-22-001",
  ol.agent.run_id = "run-2026-02-22-001"
)
```

## Suggested team rule

- Do not accept agent-generated results without a snapshot label and lineage record
- Keep one snapshot per meaningful iteration boundary
- Record prompt/run IDs in metadata for auditability

## Validation checkpoint

Before sharing results:

```r
lake <- Lake$new("analysis_project")
lake$log()
lake$tree("analysis_summary")
```

## Minimal acceptance criteria

- snapshot exists for the run
- lineage tree resolves upstream dependencies
- reproducibility metadata includes git/renv/session context
