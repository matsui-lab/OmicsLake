# User Journeys

This page gives visual, role-based paths for adopting OmicsLake.

## Journey A: First-time analyst

```mermaid
flowchart LR
  A[Install package] --> B[Run first_run_check.sh]
  B --> C[Read START_HERE]
  C --> D[Run canonical RNA-seq demo]
  D --> E[Adopt in own project]
```

## Journey B: Existing pipeline owner

```mermaid
flowchart LR
  A[Existing script] --> B[Add track_script]
  B --> C[Execute unchanged workflow]
  C --> D[Inspect tree and log]
  D --> E[Add snapshots for milestones]
```

## Journey C: Team reproducibility lead

```mermaid
flowchart LR
  A[Enable strict mode] --> B[Capture git and renv metadata]
  B --> C[Track script and pipeline runs]
  C --> D[Validate snapshots]
  D --> E[Share lineage-aware handoff]
```

## Journey D: AI-assisted coding workflow

```mermaid
flowchart LR
  A[Set agent metadata options] --> B[Run track_script or track_pipeline]
  B --> C[Create snapshot per iteration]
  C --> D[Review log and lineage]
  D --> E[Publish reproducible result]
```

## Commands used across journeys

```bash
bash tools/first_run_check.sh
bash tools/run_demo_count_edger_limma_voom_ora.sh
```
