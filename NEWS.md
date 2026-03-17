# OmicsLake NEWS

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
