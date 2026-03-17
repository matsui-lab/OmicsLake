# R-universe submission notes

This folder contains files to onboard `OmicsLake` to R-universe.

## 1) Create the universe repo

Create a public repository named:

`matsui-lab.r-universe.dev`

## 2) Add package registry file

Copy `tools/r-universe/packages.json` from this repository into the root of
`matsui-lab.r-universe.dev`.

## 3) Enable the R-universe GitHub app

Install/authorize the R-universe app for the `matsui-lab` organization and
grant access to:

- `matsui-lab/OmicsLake`
- `matsui-lab/matsui-lab.r-universe.dev`

## 4) Verify build

After onboarding, package pages should appear at:

- https://matsui-lab.r-universe.dev
- https://matsui-lab.r-universe.dev/OmicsLake

## Optional: automate with gh CLI

If GitHub CLI auth is valid, run:

```bash
bash tools/r-universe/submit_to_runiverse.sh
```
