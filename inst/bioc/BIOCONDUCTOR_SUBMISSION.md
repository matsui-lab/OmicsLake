# Bioconductor Submission Checklist (OmicsLake)

This checklist captures the package-level work needed before opening a
Bioconductor submission issue.

## 1. Run local checks

```bash
Rscript tools/check_bioc_readiness.R
Rscript -e "testthat::test_local(reporter='summary')"
./tools/check_bioc.sh
```

Expected:
- no `ERROR` in `R CMD check`
- no `ERROR` in `BiocCheck`
- package version follows Bioconductor devel style (e.g., odd middle version: `0.99.0`)

Notes:
- `tools/check_bioc.sh` writes BiocCheck logs to `/tmp/omicslake-bioccheck` by default.
- Support-site registration check is disabled by default for local CI stability.
- Enable full BiocCheck account checks with:
  - `NO_CHECK_BIOC_HELP=0 ./tools/check_bioc.sh`
- Optional local gating toggles for advisory notes:
  - `NO_CHECK_FUNCTION_LEN=1` skips function-length recommendation checks.
  - `NO_CHECK_FORMATTING=1` skips formatting recommendation checks.
  - Example:
    - `NO_CHECK_FUNCTION_LEN=1 NO_CHECK_FORMATTING=1 ./tools/check_bioc.sh`

Latest local run (2026-07-15, OmicsLake 0.99.3):
- `R CMD check`: `Status: OK`
- `BiocCheck`: `0 ERRORS | 0 WARNINGS | 8 NOTES`
- Remaining BiocCheck NOTE categories are advisory coding-style checks,
  including function length, line length, indentation, and console output.

## 2. Verify metadata in DESCRIPTION

- `Title`, `Description`, `URL`, and `BugReports` are final
- `biocViews` are appropriate and specific
- `VignetteBuilder: knitr` is present

## 3. Verify vignette quality

- At least one end-to-end workflow vignette is present under `vignettes/`
- Vignettes render without manual intervention
- Narrative clearly explains expected inputs/outputs and reproducibility options

## 4. Reproducibility policy for users

Confirm docs explain:
- `ol_enable_strict_repro_mode()` / `lake_strict_on()`
- `ol.repro.require_clean_git`
- `ol.snapshot.validate.mode`

## 5. CI status

GitHub Actions should pass:
- `R-CMD-check`
- `bioc-check`

## 6. Submission package hygiene

- No generated tarballs or `.Rcheck` directories committed
- No project-local artifacts in package root
- No unresolved TODO/FIXME in exported user-facing APIs

## 7. Bioconductor submission status

- Submission issue: https://github.com/Bioconductor/BiocContributions/issues/97
- Policies accepted and the package was cloned to BiocStaging.
- The first external build identified two optional-adapter test assumptions and
  one Windows-only path assertion. OmicsLake 0.99.2 corrected those checks.
- OmicsLake 0.99.3 standardizes optional agent-context capture on the
  package-specific `OL_*` environment variables and uses neutral
  software-agent terminology in tests and documentation.

The tracker and BiocStaging builds should be monitored after each devel update.

## 8. Maintainer account checks (manual)

Before final submission issue creation, confirm with the maintainer email in `DESCRIPTION`:
- Bioconductor support site account exists and is searchable
- bioc-devel mailing list registration is complete
