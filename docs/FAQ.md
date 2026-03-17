# FAQ

## Is OmicsLake only for large teams?

No. It works for solo analyses, small collaborations, and large projects.

## Do I need to rewrite my pipeline?

No. Start with `track_script()` or `track_pipeline()` to keep existing scripts unchanged.

## Do I need SQL knowledge?

No. You can use formula syntax, dplyr pipelines, and QueryBuilder.

## Will this slow down normal analysis?

For many workflows, overhead is small relative to analysis runtime. Use lightweight tracking first and scale up.

## Can I use Bioconductor objects?

Yes. OmicsLake supports *SummarizedExperiment* and *MultiAssayExperiment* workflows.

## How do I enforce strict reproducibility?

Use:

```r
ol_enable_strict_repro_mode(path = getwd())
```

This enables strict checks and richer metadata capture.

## How do I run the canonical count -> edgeR -> limma-voom -> ORA flow?

```bash
bash tools/first_run_check.sh
bash tools/run_demo_count_edger_limma_voom_ora.sh
```

## What if installation fails?

See `docs/TROUBLESHOOTING.md`.

## Can this work with AI-assisted coding?

Yes. See `docs/AI_AGENT_INTEGRATION.md` for consistency controls and run metadata handling.
