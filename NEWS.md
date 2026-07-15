# OmicsLake NEWS

## OmicsLake 0.99.3

- Standardized optional agent-context capture on the package-specific
  `OL_PROMPT_ID`, `OL_AGENT_RUN_ID`, and `OL_AGENT_NAME` environment
  variables.
- Replaced tool-specific labels in tests and documentation with neutral
  software-agent terminology.

## OmicsLake 0.99.2

- Declared the optional `xcms` and `Chromatograms` adapter dependencies and
  aligned dependency-contract tests with the adapters' runtime behavior.
- Made path-normalization testing portable across Unix-like and Windows hosts.
- Expanded the package description and removed the premature package citation;
  a publication citation will be added when a stable DOI is available.

## OmicsLake 0.99.1

- Prepared the public Bioconductor submission snapshot.
- Isolated test storage in a temporary directory for reliable package checks.
- Preserved callers' random-number state in evaluation helpers.
- Completed missing evaluation-topic return-value documentation.

## OmicsLake 0.99.0

- Bioconductor submission preparation release.
- Added reproducibility hardening:
  - strict reproducibility mode (`ol_enable_strict_repro_mode()`, `lake_strict_on()`)
  - automatic Git/renv/session metadata capture on commit/snapshot
  - optional clean-git guard and snapshot validation checks
- Added Bioconductor preflight tooling:
  - `tools/check_bioc.sh`
  - `tools/check_bioc_readiness.R`
  - GitHub Actions workflow `.github/workflows/bioc-check.yml`
- Expanded documentation and submission checklist in `inst/bioc/BIOCONDUCTOR_SUBMISSION.md`.
