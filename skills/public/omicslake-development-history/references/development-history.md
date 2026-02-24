# OmicsLake Development History

Last updated: 2026-02-24

## Scope

This file records major technical and manuscript decisions made during OmicsLake development, with emphasis on:

- reproducibility semantics (`snap`/`restore`, lineage granularity)
- benchmark interpretation and fairness language
- reviewer-driven manuscript revisions for GigaScience submission
- synchronization state of TeX/PDF/figure bundle artifacts

## Chronology

### 1) Package hardening and reproducibility feature expansion

- Goal:
  - improve package usability for integration into existing bioinformatics workflows
  - strengthen reproducibility controls and repair diagnostics
- Key outcomes:
  - expanded reproducibility and tracking utilities
  - strengthened bridge with practical workflow usage patterns (count -> edgeR -> limma-voom -> ORA demos and related guidance)
  - improved package docs/vignettes and user guidance

### 2) Bioconductor-oriented reorganization and submission readiness work

- Goal:
  - reorganize package and checks toward Bioconductor expectations
- Key outcomes:
  - Bioconductor-facing documentation and check scripts were added
  - submission support files were consolidated under `inst/bioc` and `tools`
  - repeated readiness checks and iterative fixes were performed

### 3) Reproducibility evaluation framework formalization

- Goal:
  - make reproducibility claims defensible with explicit test protocols
- Key outcomes:
  - RT-style validation structure (RT-001 to RT-005) was formalized in scripts and manuscript
  - breakage taxonomy, scorecard, and stress/limit evaluations were added
  - comparison framing was reworked to avoid unsupported numeric claims for baseline workflows

### 4) Manuscript and figure pipeline build-out

- Goal:
  - make paper figures and tables reproducible from scripts
- Key outcomes:
  - figure generation pipeline consolidated in `inst/paper/figures/`
  - manuscript tied to generated artifacts in `inst/paper/manuscript/main.tex`
  - submission bundle generation and manifests added

### 5) Reviewer-round fixes (major themes)

- Lineage granularity:
  - fixed narrative inconsistency by defining lineage as dataset-name level.
  - strict historical restoration is described as reference/snapshot resolution (`__ol_refs`, `__ol_objects`).
- Immutability/storage semantics:
  - clarified separation of active working state vs restorable labeled state.
  - clarified that table snapshot semantics involve in-database backup table materialization.
- Benchmark fairness:
  - removed problematic p-value display in main table.
  - clarified snapshot baseline as file-copy proxy with different semantics.
- Baseline reproducibility percentages:
  - removed estimated numeric baseline percentages.
  - kept baseline comparisons qualitative; OmicsLake metrics linked to experimental RT outcomes.
- FAIR wording:
  - changed from "FAIR-compliant" to "FAIR-aligned" and scoped full FAIR compliance to external PID/deposition infrastructure.
- Cross-environment claim:
  - normalized wording to simulated transfer boundaries.

### 6) Numeric and wording corrections (late-stage)

- Floating-point tolerance explanation:
  - removed incorrect "100 x machine epsilon" wording.
  - replaced with `1e-8` being on the order of `sqrt(.Machine$double.eps)`.
- `all.equal` notation:
  - normalized to code-style `all.equal(tolerance = 1e-8)` and `tol=1e-8`.
- attribute comparison:
  - explicitly explained why `check.attributes = FALSE` is used for non-semantic backend/class metadata differences.

### 7) Benchmark rerun and synchronization fixes

- `inst/paper/03_actual_benchmarks.R` was rerun to regenerate benchmark outputs.
- Important correction:
  - snapshot benchmark target was corrected to `lake$snap()` (from earlier inconsistent target).
- Current representative values (from `results/Table3_final.csv`):
  - Import: 36.55x
  - Aggregation: 6.23x
  - Join: 6.23x
  - Snapshot: 0.23x (slower than file-copy proxy)
- `Figure3` and manuscript table/caption wording were aligned with empirical run-interval interpretation.

### 8) Storage-layer wording harmonization (latest reviewer pass)

- Main decision:
  - describe DuckDB as primary local persistence + query execution.
  - describe Parquet as export/interchange for shareable tabular artifacts.
- Where applied:
  - Abstract findings text
  - Architecture section
  - Figure 1 caption
  - FAIR accessibility paragraph
  - storage efficiency interpretation
  - figure short captions / submission manifest wording

### 9) Round-6 reviewer-comment resolution and PDF synchronization (2026-02-23)

- Trigger: reviewer feedback identifying TeX/PDF mismatch and additional improvement points.
- Changes:
  - M1: Added snap() physical-materialization explanation and benchmark coherence statement.
  - M3: Added NVMe SSD, warm-up protocol (3 iterations), page-cache policy, determinism statement.
  - m1: Removed "Figure X:" prefix from 4 figure R scripts (figure2, figure4, figure5, figure6).
  - m2: Changed "deterministic cleanup" to "automatic cleanup when garbage-collected."
- Files changed:
  - `inst/paper/manuscript/main.tex` (3 edits: lines 227, 644-645, 648)
  - `inst/paper/figures/figure2_lineage_workflow.R` (title)
  - `inst/paper/figures/figure4_reproducibility_workflow.R` (title)
  - `inst/paper/figures/figure5_reproducibility_comparison.R` (title)
  - `inst/paper/figures/figure6_storage_efficiency.R` (title)
  - `inst/paper/manuscript/RESPONSE_TO_REVIEWERS_GIGASCIENCE.md` (Round-6 appended)
- Regenerated:
  - 4 figure PDFs (figures 2, 4, 5, 6) via Rscript
  - Submission bundle updated (`submission_bundle_gigascience/`)
  - `main.pdf` recompiled via pdflatex+bibtex 3-pass
- Verification: 5-point PDF checkpoint confirmed (no immutable, no cryptographic claims, no 40/70%, no zero-copy snapshots, figures use qualitative labels).
- TeX/PDF synchronized: YES
- Bundle manifest regenerated: figures copied, manifest unchanged (filenames stable)

### 10) Round-7 reviewer-comment resolution (2026-02-23)

- Trigger: reviewer identified version inconsistency (..Rcheck/ artifacts with 92.5× vs current 0.23×) and requested benchmark table/text improvements.
- Critical fix: deleted `..Rcheck/` directory (175MB stale R package check artifacts containing old benchmark numbers).
- Changes:
  - Table 6: snapshot speedup replaced with "---" + footnote explaining non-comparability.
  - Table 6 footnotes: added measurement scope (collect() included), warm-cache note, bold "not semantically identical" footnote.
  - Figure 3: snapshot annotation changed from "0.23x" to "N/C" (not comparable).
  - Benchmark text: added cold-cache caveat, removed redundant ratio text.
  - Discussion/Generalizability: added specific untested types (dgCMatrix, DelayedArray, HDF5-backed).
  - Discussion/Environmental Scope: added specific risk layers (DuckDB binary, Arrow ABI, Parquet codec).
  - Data Availability: improved PID wording ("expedite deposition upon request").
  - RESPONSE_TO_REVIEWERS: added Round-7 with section-number cross-references for all prior items.
- Files changed:
  - `inst/paper/manuscript/main.tex` (Table 6, benchmark text, Discussion, Data Availability)
  - `inst/paper/figures/figure3_performance_benchmarks.R` (snapshot annotation N/C)
  - `inst/paper/figures/output/submission_bundle_gigascience/Figure3_PerformanceBenchmarks.pdf`
  - `inst/paper/manuscript/RESPONSE_TO_REVIEWERS_GIGASCIENCE.md` (Round-7)
- Verification: 12-point comprehensive check (all PASS).
- TeX/PDF synchronized: YES

### 11) Round-8 reviewer-comment resolution (2026-02-23)

- Trigger: reviewer provided a checked TeX file (`main_checked.tex`) with 6 major comments and minor suggestions. The checked file was based on an older pre-Round-6 version of main.tex.
- Key finding: 5 of 6 major comments were already resolved by Rounds 6-7 (the reviewer was checking an outdated version).
  - (1) Figure 3 "95% confidence intervals" → already uses "empirical 2.5th--97.5th percentile interval"
  - (2) "100× machine epsilon" → already replaced with `sqrt(.Machine$double.eps)` explanation
  - (3) "60-70%/30-40%" estimates → already removed; no percentage estimates in current text
  - (5) Git LFS/memoise citations → already present with `\citep{}` (the checked file had removed them)
  - (6) Snapshot benchmark detail → already includes CREATE TABLE ... AS SELECT explanation
- Changes:
  - Added Supplementary Figure S2 (Major Results Dashboard) figure environment and subsection.
  - Improved Section 2.4 (Practical Impact) paragraph: "To quantify" → "To illustrate"; simplified bullet items to qualitative phase descriptions.
  - RESPONSE_TO_REVIEWERS: appended Round-8 with status of all 6 comments.
- Files changed:
  - `inst/paper/manuscript/main.tex` (3 edits: FigureS2 environment, Supplementary S2 subsection, Practical Impact paragraph)
  - `inst/paper/manuscript/RESPONSE_TO_REVIEWERS_GIGASCIENCE.md` (Round-8)
- Verification: 16-point comprehensive check (all PASS).
- TeX/PDF synchronized: YES

### 12) Round-9 final polish (2026-02-23)

- Trigger: detailed reviewer-style feedback identifying remaining adoption risks and minor issues.
- Changes (priority A — high):
  - Import benchmark defense: added explicit measurement definition (Parquet read → DuckDB registration → collect()), throughput annotation (~7.1 GB/s under warm-cache), updated Table 6 footnote with import scope.
  - Figure 3 caption: added cold-cache caveat cross-referencing Section 3.1.
- Changes (priority B — medium-high):
  - Snapshot storage cost: added new paragraph in Discussion (Section 4.2) explaining physical-copy storage growth, recommended pruning workflow, and planned `ol_prune()` utility.
- Changes (priority C — medium, already resolved):
  - Confirmed no quantitative tracking estimates ("60-70%") remain; all qualitative.
- Changes (priority D — medium):
  - SE round-trip fidelity: added verification table (Table S4) in Supplementary covering 5 test configurations (single assay, multi-assay, factor columns, nested metadata, gene annotations).
- Changes (minor):
  - lstset comment: fixed contradictory comment (breakatwhitespace=false now says "mid-word line breaks").
  - API table: "version/tag" → "snapshot label or relative reference" for terminology consistency.
- Response to Reviewers:
  - Complete rewrite in point-by-point format (Reviewer Comment → Response → Location) with 12 major items, 6 minor items, and section cross-reference table.
- Files changed:
  - `inst/paper/manuscript/main.tex` (8 edits: lstset, API table, import definition+throughput, Table 6 footnote, Figure 3 caption, snapshot storage paragraph, SE round-trip table)
  - `inst/paper/manuscript/RESPONSE_TO_REVIEWERS_GIGASCIENCE.md` (complete rewrite in point-by-point format)
- Verification: 21-point comprehensive check (20 PDF + 1 source-only = all PASS).
- TeX/PDF synchronized: YES

### 13) Crash-resume execution hardening and checklist progression (2026-02-24)

- Trigger: terminal session interruption during submission-readiness progression; user requested ordered resume.
- Changes:
  - Manuscript reference integrity:
    - added explicit references to `tab:tool_comparison` and `tab:rt003_config` in `main.tex`.
    - re-validated all `fig:` and `tab:` labels are referenced.
  - Benchmark script hardening (`inst/paper/01_performance_benchmark.R`):
    - fixed `bench::mark` expression matching by coercing expressions to character.
    - replaced failing dynamic splicing (`!!!`) with `do.call(bench::mark, ...)`.
    - made bench column printing version-safe (optional `max` column).
    - fixed fair-join context by initializing/writing join tables before DuckDB query.
    - fixed BM-006 restore-reference logic by switching from commit-id refs to explicit table tags (`ol_tag`).
  - Re-executed benchmark/reproducibility scripts in README order:
    - `00_generate_datasets.R`
    - `01_performance_benchmark.R`
    - `02_reproducibility_test.R`
    - `04_breakage_taxonomy_validation.R`
    - `06_limit_stress_test.R`
    - `05_reproducibility_scorecard.R`
  - Container check:
    - ran `05_reproducibility_scorecard.R` inside `bioconductor/bioconductor_docker:RELEASE_3_21`.
  - Checklist update:
    - checked internal items completed by this run in `SUBMISSION_CHECKLIST.md` (figure/table references, reproducibility execution, container test, FAIR/data-availability/competing-interests/authors/acknowledgements sections).
  - TeX/PDF sync:
    - rebuilt manuscript (`pdflatex + bibtex + pdflatex + pdflatex`).
- Files changed:
  - `inst/paper/manuscript/main.tex`
  - `inst/paper/01_performance_benchmark.R`
  - `inst/paper/SUBMISSION_CHECKLIST.md`
  - benchmark/reproducibility outputs under `inst/paper/` and `inst/paper/results/`
- Verification commands and outcomes:
  - label/reference integrity check via `awk` over `main.tex`: `OK: all figure/table labels are referenced`.
  - reproducibility suite:
    - `02_reproducibility_test.R`: `Overall Result: ALL TESTS PASSED (5/5)`.
    - `04_breakage_taxonomy_validation.R`: completed with output files generated.
    - `06_limit_stress_test.R`: completed with output files generated.
    - `05_reproducibility_scorecard.R`: completed and scorecard files generated.
  - container run:
    - `docker run ... bioconductor/bioconductor_docker:RELEASE_3_21 Rscript inst/paper/05_reproducibility_scorecard.R` exited `0`.
  - TeX/PDF timestamps after rebuild:
    - `main.tex`: 2026-02-24 22:42:49
    - `main.pdf`: 2026-02-24 23:30:06
- TeX/PDF synchronized: YES
- Residual risk:
  - external submission-process items remain (Zenodo/Code Ocean DOI minting, RRID/ORCID completion, cover letter).
  - benchmark outputs changed on rerun; manuscript tables/captions should be re-checked against regenerated CSVs before final upload.

### 14) Table-2 export semantics correction after rerun (2026-02-24)

- Trigger: post-rerun consistency check found misleading summary formatting in generated Table 2 CSV exports.
- Changes:
  - fixed storage savings baseline calculation in `01_performance_benchmark.R` from `max(Size_MB)` to explicit RDS baseline.
  - changed combined Table 2 snapshot improvement wording to `N/C (semantics differ)` to avoid ratio claims for non-identical operations.
  - changed storage improvement wording to signed interpretation (`x% smaller` / `x% larger`) based on actual measured direction.
  - hardened optional data.table branch checks via character-cast expression matching.
- Files changed:
  - `inst/paper/01_performance_benchmark.R`
  - regenerated:
    - `inst/paper/results/Table2B_storage_efficiency.csv`
    - `inst/paper/results/Table2_paper_format.csv`
- Verification command and outcome:
  - `Rscript inst/paper/01_performance_benchmark.R` completed successfully (exit 0).
  - Export validation:
    - `Table2B_storage_efficiency.csv`: Parquet `Savings_Percent = -19.3`.
    - `Table2_paper_format.csv`: snapshot row shows `N/C (semantics differ)`; storage row shows `19.3% larger`.
- Residual risk:
  - manuscript main text still follows canonical submission values (`results/Table3_final.csv` / figure pipeline), not the legacy Table2 export script outputs; keep this boundary explicit in final submission QA.

## Current State Snapshot

As of 2026-02-23:

- manuscript source:
  - `inst/paper/manuscript/main.tex`
- compiled manuscript:
  - `inst/paper/manuscript/main.pdf`
- reviewer response map:
  - `inst/paper/manuscript/RESPONSE_TO_REVIEWERS_GIGASCIENCE.md`
- benchmark outputs:
  - `results/Table3_final.csv`
  - `inst/paper/results_extended_comparison.csv`
- submission figures manifest:
  - `inst/paper/figures/output/submission_bundle_gigascience/submission_manifest.csv`

## Known Residual Risks

- DOI minting (Zenodo/Code Ocean) and external registry IDs (RRID/bio.tools) remain external process items.
- LaTeX warning noise (overfull/duplicate destination) remains but does not block PDF generation.
- Ongoing edits must keep TeX/PDF and bundle captions synchronized to avoid submission inconsistency.

## Update Procedure

When new development happens:

1. append a new dated subsection in this file
2. list exact files changed
3. include verification command(s) and key output
4. state whether TeX/PDF and bundle manifest were regenerated
