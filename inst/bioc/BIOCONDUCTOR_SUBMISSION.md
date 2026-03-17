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

Latest local run (2026-02-22):
- `R CMD check`: `Status: OK`
- `BiocCheck`: `0 ERRORS | 0 WARNINGS | 3 NOTES`
- Remaining NOTE categories:
  - function length recommendation (>50 lines)
  - long line formatting (>80 chars)
  - indentation formatting (non-4-space multiples)

Advisory-note-suppressed local run (2026-02-22):
- `NO_CHECK_FUNCTION_LEN=1 NO_CHECK_FORMATTING=1 ./tools/check_bioc.sh`
- `R CMD check`: `Status: OK`
- `BiocCheck`: `0 ERRORS | 0 WARNINGS | 0 NOTES`

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

## 7. Open Bioconductor submission

Open a new package issue at:
https://github.com/Bioconductor/Contributions/issues/new/choose

Include:
- package summary (scope and novelty)
- main workflow vignette link
- repository URL and CI badges
- expected user community / use cases

## 8. Maintainer account checks (manual)

Before final submission issue creation, confirm with the maintainer email in `DESCRIPTION`:
- Bioconductor support site account exists and is searchable
- bioc-devel mailing list registration is complete
