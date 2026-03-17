#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEMO_SCRIPT="${ROOT_DIR}/tools/demo_count_edger_limma_voom_ora.R"

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  cat <<'USAGE'
Usage:
  bash tools/run_demo_count_edger_limma_voom_ora.sh

Environment variables:
  AUTO_INSTALL=1   Install missing dependencies automatically (default: 1)
  AUTO_INSTALL=0   Fail if dependencies are missing

What this runs:
  count -> edgeR -> limma-voom -> ORA (goana)
  plus OmicsLake lineage/snapshot recording.
USAGE
  exit 0
fi

if [[ ! -f "${DEMO_SCRIPT}" ]]; then
  echo "ERROR: demo script not found: ${DEMO_SCRIPT}"
  exit 1
fi

AUTO_INSTALL="${AUTO_INSTALL:-1}"
export OMICSLAKE_DEMO_ROOT="${ROOT_DIR}"

echo "==> Preparing dependencies (AUTO_INSTALL=${AUTO_INSTALL})"
Rscript - <<'RS'
auto_install <- identical(Sys.getenv("AUTO_INSTALL", "1"), "1")
root <- Sys.getenv("OMICSLAKE_DEMO_ROOT")

install_cran_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) return(invisible(NULL))
  if (!auto_install) {
    stop("Missing CRAN packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  install.packages(missing, repos = "https://cloud.r-project.org")
}

install_bioc_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) return(invisible(NULL))
  if (!auto_install) {
    stop("Missing Bioconductor packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  }
  BiocManager::install(missing, ask = FALSE, update = FALSE)
}

install_cran_if_missing(c("remotes"))
if (!requireNamespace("OmicsLake", quietly = TRUE)) {
  if (!auto_install) {
    stop("OmicsLake is not installed. Set AUTO_INSTALL=1 or install manually.", call. = FALSE)
  }
  remotes::install_local(root, upgrade = "never", dependencies = TRUE)
}

install_bioc_if_missing(c("edgeR", "limma", "org.Hs.eg.db", "AnnotationDbi"))
RS

echo "==> Running canonical demo: count -> edgeR -> limma-voom -> ORA"
Rscript "${DEMO_SCRIPT}"

echo "==> Demo finished"
