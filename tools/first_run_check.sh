#!/usr/bin/env bash
set -euo pipefail

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  cat <<'USAGE'
Usage:
  bash tools/first_run_check.sh

Purpose:
  Run a fast local sanity check for first-time OmicsLake users.

Checks:
  1) package load
  2) basic put/get
  3) dplyr-based lineage capture
  4) snapshot creation
USAGE
  exit 0
fi

echo "==> Running first-run sanity check"

Rscript - <<'RS'
if (!requireNamespace("OmicsLake", quietly = TRUE)) {
  stop("OmicsLake is not installed. Install it first (see README).", call. = FALSE)
}

suppressPackageStartupMessages(library(OmicsLake))
suppressPackageStartupMessages(library(dplyr))

root <- file.path(tempdir(), "omicslake_first_run_root")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
options(ol.root = root)

project <- paste0("first_run_check_", format(Sys.time(), "%Y%m%d_%H%M%S"))
lake <- Lake$new(project)

lake$put("tiny", data.frame(sample_id = 1:3, value = c(10, 20, 30)))
out <- lake$get("tiny")
if (!is.data.frame(out) || nrow(out) != 3L) {
  stop("Basic put/get check failed.", call. = FALSE)
}

lake$ref("tiny") |>
  mutate(value2 = value * 2) |>
  save_as("tiny2", lake)

deps <- lake$deps("tiny2", direction = "up")
if (!is.data.frame(deps) || nrow(deps) < 1L) {
  stop("Lineage capture check failed.", call. = FALSE)
}

lake$snap("first_run_ok", note = "first run sanity check")

cat("OK: first-run sanity check passed\n")
cat("Project:", project, "\n")
cat("Root:", root, "\n")
cat("Next: run 'bash tools/run_demo_count_edger_limma_voom_ora.sh'\n")
RS

echo "==> Done"
