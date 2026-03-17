## Test environments

- macOS Sequoia 15.6 (aarch64), R 4.5.2

## R CMD check results

- `R CMD check --as-cran OmicsLake_0.1.0.tar.gz`
  - 0 errors | 0 warnings | 0 notes (with local non-network incoming settings)

## Notes

- In this execution environment, external network access to CRAN/GitHub is restricted.
- For local reproducibility, I used:
  - `_R_CHECK_CRAN_INCOMING_REMOTE_=false`
  - `_R_CHECK_SYSTEM_CLOCK_=0`
  - `_R_CHECK_RD_VALIDATE_RD2HTML_=false`
- With default remote incoming checks enabled, URL/incoming notes may appear in this offline environment only.
