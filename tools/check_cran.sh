#!/usr/bin/env bash
set -euo pipefail

# Reproducible local CRAN-style check.
# Usage:
#   bash tools/check_cran.sh
#
# Override env vars if needed:
#   FORCE_SUGGESTS=true|false
#   CHECK_INCOMING_REMOTE=true|false
#   CHECK_SYSTEM_CLOCK=0|1
#   CHECK_RD_VALIDATE_RD2HTML=true|false

FORCE_SUGGESTS="${FORCE_SUGGESTS:-true}"
CHECK_INCOMING_REMOTE="${CHECK_INCOMING_REMOTE:-false}"
CHECK_SYSTEM_CLOCK="${CHECK_SYSTEM_CLOCK:-0}"
CHECK_RD_VALIDATE_RD2HTML="${CHECK_RD_VALIDATE_RD2HTML:-false}"

echo "==> Building source tarball"
R CMD build .
PKG_TARBALL="$(ls -t OmicsLake_*.tar.gz | head -n 1)"
if [[ -z "${PKG_TARBALL}" ]]; then
  echo "ERROR: no source tarball produced."
  exit 1
fi

echo "==> Running R CMD check --as-cran"
_R_CHECK_FORCE_SUGGESTS_="${FORCE_SUGGESTS}" \
_R_CHECK_CRAN_INCOMING_REMOTE_="${CHECK_INCOMING_REMOTE}" \
_R_CHECK_SYSTEM_CLOCK_="${CHECK_SYSTEM_CLOCK}" \
_R_CHECK_RD_VALIDATE_RD2HTML_="${CHECK_RD_VALIDATE_RD2HTML}" \
R CMD check --as-cran "${PKG_TARBALL}"

echo "==> Done"
