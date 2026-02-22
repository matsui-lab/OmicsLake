#!/usr/bin/env bash
set -euo pipefail

# Bioconductor-style preflight:
# 1) build source tarball
# 2) run R CMD check (non-CRAN mode)
# 3) run BiocCheck on source tarball

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT_DIR}"

echo "==> Running Bioconductor readiness preflight"
Rscript tools/check_bioc_readiness.R

BIOCCHECK_OUT_DIR="${BIOCCHECK_OUT_DIR:-/tmp/omicslake-bioccheck}"
NO_CHECK_BIOC_HELP="${NO_CHECK_BIOC_HELP:-1}"
NO_CHECK_FUNCTION_LEN="${NO_CHECK_FUNCTION_LEN:-0}"
NO_CHECK_FORMATTING="${NO_CHECK_FORMATTING:-0}"

echo "==> Cleaning previous local BiocCheck artifacts"
rm -rf "${ROOT_DIR}/OmicsLake.BiocCheck" "${BIOCCHECK_OUT_DIR}"
mkdir -p "${BIOCCHECK_OUT_DIR}"

echo "==> Building source tarball"
R CMD build .

PKG_TARBALL="$(ls -t OmicsLake_*.tar.gz | head -n 1)"
if [[ -z "${PKG_TARBALL}" ]]; then
  echo "ERROR: no source tarball produced."
  exit 1
fi

echo "==> Running R CMD check"
R CMD check "${PKG_TARBALL}"

if [[ "${SKIP_BIOCCHECK:-0}" == "1" ]]; then
  echo "==> SKIP_BIOCCHECK=1 set; skipping BiocCheck."
  echo "==> Done"
  exit 0
fi

echo "==> Running BiocCheck"
Rscript -e "if(!requireNamespace('BiocManager', quietly=TRUE)) install.packages('BiocManager', repos='https://cloud.r-project.org')"
Rscript -e "if(!requireNamespace('BiocCheck', quietly=TRUE)) BiocManager::install('BiocCheck', ask=FALSE, update=FALSE)"

if [[ "${NO_CHECK_BIOC_HELP}" == "1" ]]; then
  echo "==> BiocCheck support-site check disabled (set NO_CHECK_BIOC_HELP=0 to enable)"
else
  echo "==> BiocCheck support-site check enabled"
fi

export OL_BIOC_TARBALL="${PKG_TARBALL}"
export OL_BIOC_CHECK_DIR="${BIOCCHECK_OUT_DIR}"
export OL_NO_CHECK_BIOC_HELP="${NO_CHECK_BIOC_HELP}"
export OL_NO_CHECK_FUNCTION_LEN="${NO_CHECK_FUNCTION_LEN}"
export OL_NO_CHECK_FORMATTING="${NO_CHECK_FORMATTING}"
Rscript - <<'RS'
tarball <- Sys.getenv("OL_BIOC_TARBALL")
check_dir <- Sys.getenv("OL_BIOC_CHECK_DIR")
no_help <- identical(Sys.getenv("OL_NO_CHECK_BIOC_HELP"), "1")
no_fun_len <- identical(Sys.getenv("OL_NO_CHECK_FUNCTION_LEN"), "1")
no_format <- identical(Sys.getenv("OL_NO_CHECK_FORMATTING"), "1")

args <- c(
  list(package = tarball, checkDir = check_dir),
  setNames(list(TRUE), "quit-with-status")
)
if (no_help) {
  args[["no-check-bioc-help"]] <- TRUE
}
if (no_fun_len) {
  args[["no-check-function-len"]] <- TRUE
}
if (no_format) {
  args[["no-check-formatting"]] <- TRUE
}

do.call(BiocCheck::BiocCheck, args)
RS

echo "==> Done"
