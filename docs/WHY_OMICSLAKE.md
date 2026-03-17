# Why OmicsLake

## Core value proposition

OmicsLake lets you keep your existing analysis style while adding lineage and reproducibility as first-class outputs.

## What it solves

- Losing provenance when scripts are edited repeatedly
- Difficulty reproducing old intermediate results
- Ambiguous handoff between collaborators
- AI-assisted code changes without durable run metadata

## Design principle

Do not force users to rewrite their workflows.

## Practical differentiators

- script-first integration (`track_script`, `track_pipeline`)
- dplyr-native dependency capture
- snapshot/tag rollback workflow
- strict reproducibility mode with git/renv/session metadata

## When not to use

- if you only need one-off in-memory analysis without persistence
- if your environment disallows local file/database writes

## Recommended rollout path

1. Start with one existing script and track it unchanged
2. Add snapshot labels at meaningful milestones
3. Enable strict reproducibility mode once team flow is stable
4. Expand to larger pipelines and team handoff checkpoints
