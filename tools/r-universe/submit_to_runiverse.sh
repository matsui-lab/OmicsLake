#!/usr/bin/env bash
set -euo pipefail

# Submit/update this package in R-universe via GitHub.
#
# Prerequisites:
# - gh CLI is installed
# - gh auth login is completed with valid token
#
# Usage:
#   bash tools/r-universe/submit_to_runiverse.sh
#   ORG=matsui-lab PKG_REPO=OmicsLake bash tools/r-universe/submit_to_runiverse.sh

ORG="${ORG:-matsui-lab}"
PKG_REPO="${PKG_REPO:-OmicsLake}"
UNIVERSE_REPO="${ORG}.r-universe.dev"

gh auth status >/dev/null

if ! gh repo view "${ORG}/${UNIVERSE_REPO}" >/dev/null 2>&1; then
  gh repo create "${ORG}/${UNIVERSE_REPO}" --public --description "R-universe registry for ${ORG}"
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "${tmpdir}"' EXIT

gh repo clone "${ORG}/${UNIVERSE_REPO}" "${tmpdir}/${UNIVERSE_REPO}"

cat > "${tmpdir}/${UNIVERSE_REPO}/packages.json" <<JSON
{
  "packages": {
    "${PKG_REPO}": {
      "url": "https://github.com/${ORG}/${PKG_REPO}"
    }
  }
}
JSON

(
  cd "${tmpdir}/${UNIVERSE_REPO}"
  git add packages.json
  if ! git diff --cached --quiet; then
    git commit -m "Add ${PKG_REPO} to R-universe registry"
    git push
  fi
)

echo "R-universe registry updated:"
echo "  https://${ORG}.r-universe.dev"
echo "  https://${ORG}.r-universe.dev/${PKG_REPO}"
