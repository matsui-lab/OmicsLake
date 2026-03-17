# Adoption Playbook

This playbook focuses on broad acceptance and sustained user growth.

## Positioning

Primary message:

- OmicsLake adds reproducibility and lineage to existing analysis with minimal code changes.

Secondary message:

- You can start in minutes, then scale to strict reproducibility when needed.

## User segments

- Solo analyst: wants fast setup and rollback safety
- Collaborative team: needs traceable handoff and shared context
- Platform/infra lead: needs policy-based reproducibility controls
- AI-assisted analysis user: needs run-level consistency and auditability

## Friction reduction tactics

- one-command demo: `bash tools/run_demo_count_edger_limma_voom_ora.sh`
- first-run checklist: `docs/FIRST_RUN_CHECKLIST.md`
- troubleshooting page for setup failures
- consistent naming (`Lake` API plus `lake_*` shortcuts)

## Trust building tactics

- show lineage graph examples in every major workflow
- include snapshot checkpoints in tutorials
- provide explicit reproducibility defaults (git/renv/session/system)
- keep examples runnable with realistic inputs

## Communication assets to maintain

- README with clear start path and role-based navigation
- three depth levels of vignettes: quick, practical, comprehensive
- concise FAQ and troubleshooting docs
- migration guide for legacy API users

## Release communication checklist

- summarize what changed for users
- include one copy-paste starter snippet
- include one practical workflow example
- include one reproducibility example
- include known limitations and current scope

## Success metrics (suggested)

- time-to-first-success under 10 minutes
- first-run script success rate above 90%
- issue resolution median under 72 hours
- increasing reuse of demo workflow in user reports
