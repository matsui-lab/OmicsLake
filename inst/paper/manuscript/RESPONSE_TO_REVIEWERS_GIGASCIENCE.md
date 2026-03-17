# Response to Reviewer Comments

**Manuscript**: OmicsLake: Versioned Data Management with Automatic Lineage Tracking for Reproducible Bioinformatics

**Journal**: GigaScience

**Date**: 2026-02-23

---

## Summary of Changes

This document provides point-by-point responses to all reviewer comments received across multiple review rounds. For clarity, responses are organized thematically rather than chronologically, consolidating related items that were addressed across successive rounds.

**Major revisions** include: (1) clarification of lineage granularity as dataset-name level; (2) rewritten put/snap/restore semantics separating active state from restorable labeled state; (3) comprehensive benchmark fairness improvements including p-value removal, explicit semantic baseline differences, and hardware/software disclosure; (4) removal of all estimated baseline percentages in favor of qualitative descriptions; (5) correction of FAIR wording to "FAIR-aligned"; (6) harmonized cross-environment claim language; (7) corrected floating-point tolerance rationale; (8) clarified storage layer roles (DuckDB as persistence/query engine, Parquet as export/interchange); (9) improved snapshot benchmark interpretation with semantic non-comparability notation; (10) specified untested data types and risk layers for generalizability claims; (11) improved Data Availability wording with expedited deposition commitment; and (12) unified figure caption interval wording.

**Minor revisions** include: removal of embedded figure-title prefixes from R scripts, softened garbage-collection wording, fixed lstset comment formatting, updated API table terminology, improved Practical Impact paragraph wording, and added Supplementary Figure S2.

---

## Major Items

### Comment M1: Lineage granularity

> **Reviewer comment**: The manuscript was inconsistent about whether lineage tracking operates at the dataset-name level or at the version level. It was unclear how exact historical restoration relates to lineage edges.

**Response**: Lineage tracking in OmicsLake operates at the dataset-name level in the current release. This was clarified throughout the manuscript. Exact historical restoration is handled by snapshot/object reference resolution (via `__ol_refs` and `__ol_objects` metadata tables), not by version-level lineage edges. The terminology in Section 2.1 was updated to define this distinction explicitly, and the tracking description in Section 2.3 (Automatic Dependency Tracking) was revised to match.

**Location**: Section 2.1 (Terminology), Section 2.3 (Automatic Dependency Tracking)

---

### Comment M2: put/snap/restore semantics

> **Reviewer comment**: The manuscript was ambiguous about immutability and storage semantics. The roles of `put`, `snap`, and `restore` were not clearly separated, and the physical materialization behavior of `snap()` was unclear.

**Response**: The terminology and architecture explanations were rewritten to clearly separate two concerns: (a) active working table writes (`put` creates, overwrites, or appends data), and (b) restorable labeled states (`snap`/`restore` operate via backup tables and reference mappings). A dedicated data model table listing `__ol_refs`, `__ol_objects`, and `__ol_dependencies` key fields and their roles was added. The manuscript now explicitly states that `snap()` on tables performs physical in-database materialization via `CREATE TABLE ... AS SELECT`, meaning that snapshot cost scales with captured rows rather than being metadata-only. For R objects, `snap()` creates a label-to-version mapping without payload duplication. Append-mode semantics were clarified in the Discussion.

**Location**: Section 2.3 (Data Model and Storage Semantics), Section 3.1 (Performance Benchmarks), Section 4.2 (Technical Trade-offs)

---

### Comment M3: Benchmark fairness

> **Reviewer comment**: Several concerns were raised about benchmark presentation: (a) the inclusion of p-values was questioned, (b) the snapshot baseline comparison lacked acknowledgment of semantic differences, (c) hardware/software conditions and warm-up/cache protocols were insufficiently described, and (d) the scope of import benchmarks (including full `collect()` materialization) needed clarification.

**Response**: All four concerns were addressed.

(a) The p-value column was removed from the main benchmark table (Table 6).

(b) The snapshot baseline (file-copy proxy) has fundamentally different semantics from OmicsLake's `snap()`, which performs in-database `CREATE TABLE ... AS SELECT` materialization. Table 6 now uses "---" for the snapshot speedup column with footnote b explaining: "Snapshot and file-copy proxy are not semantically identical; direct speedup ratio is omitted." Figure 3 uses "N/C" (not comparable) for the same reason. Body text reports absolute timings (0.033 s vs. 0.008 s) without computing a ratio.

(c) The benchmark environment was fully specified: Apple M4 Max, 36 GB RAM, NVMe SSD, macOS 15.6, R 4.5.2, duckdb 1.4.3, arrow 22.0.0.1, dplyr 1.1.4. The warm-up protocol was documented (3 warm-up iterations excluded from measurement). The I/O cache condition was stated: "Operating-system page cache was not cleared between iterations, reflecting typical interactive-analysis conditions." An explicit note was added that cold-start behavior would increase absolute timings for all methods and relative ordering has not been separately validated under cold-cache conditions. A determinism statement was added confirming no random-number seeds are involved.

(d) Import benchmarks include full `collect()` materialization. The measurement scope footnote in Table 6 documents this.

**Location**: Section 3.1 (Performance Benchmarks), Table 6, Figure 3

---

### Comment M4: Estimated baseline percentages removed

> **Reviewer comment**: The manuscript included estimated baseline percentages (40%/70%) for competing workflows that were not empirically verified. These should be removed or replaced with qualitative descriptions.

**Response**: All estimated numeric baseline claims were removed from the manuscript. Baseline columns in the reproducibility comparison table were converted to qualitative capability descriptors (e.g., presence/absence of a feature). Quantitative claims are now restricted to empirically verified OmicsLake reproducibility test (RT) outcomes. The Practical Impact paragraph (Section 2.4) was rewritten to use qualitative phase-level characterization instead of specific percentage estimates. The phrase "To quantify" was changed to "To illustrate" to avoid implying specific quantitative claims. Figure scripts and output captions were updated to match this qualitative baseline framing.

**Location**: Section 2.4 (Scope and Limitations of Automatic Tracking), Section 3.2 (Reproducibility Validation), Table 9, Figures 4--5

---

### Comment M5: FAIR wording

> **Reviewer comment**: The manuscript used "FAIR-compliant" wording, which overstates the level of FAIR alignment given that full compliance depends on external persistent identifier (PID) and deposition infrastructure.

**Response**: All instances of "FAIR-compliant" were replaced with "FAIR-aligned." An explicit scope statement was added noting that full FAIR compliance depends on external PID assignment and deposition infrastructure that is outside the scope of the OmicsLake package itself.

**Location**: Section 4.4 (Alignment with FAIR Principles)

---

### Comment M6: Cross-environment claims

> **Reviewer comment**: The RT-003 cross-environment reproducibility test claims were too strong given that the test uses simulated rather than physically separate environments.

**Response**: The wording was harmonized to "simulated cross-environment transfer" throughout all manuscript sections and tables that reference RT-003. The test uses logical environment pairs (distinct library paths and R session configurations on the same host) rather than physically separate machines. This limitation is now stated explicitly.

**Location**: Section 3.2.3 (Simulated Cross-Environment Transfer), Section 4.3 (Environmental Scope)

---

### Comment M7: Floating-point tolerance rationale

> **Reviewer comment**: The floating-point tolerance was incorrectly described as "100x machine epsilon." The relationship between the chosen tolerance and machine epsilon needed correction.

**Response**: The incorrect "100x machine epsilon" phrasing was removed. The tolerance is now consistently described as `1e-8`, which is on the same order as `sqrt(.Machine$double.eps)` (approximately 1.49 x 10^-8). The `all.equal` notation was unified throughout the manuscript to use code-style formatting: `all.equal(tolerance = 1e-8)`.

**Location**: Section 3.2.1 (RT-001), Section 3.2.3 (RT-003), Appendix A.4

---

### Comment M8: Storage layer wording

> **Reviewer comment**: The roles of DuckDB and Parquet in the storage architecture were not clearly distinguished, leading to potential confusion about which component serves as the primary persistence layer.

**Response**: The storage semantics were clarified consistently across the Abstract, Architecture section, and Figure 1 caption. DuckDB is now described as the primary local persistence and query engine. Parquet is described as the export/interchange format for shareable tabular artifacts. This distinction is maintained throughout all sections that reference the storage layer.

**Location**: Abstract, Section 2.1 (Architecture), Figure 1 caption

---

### Comment M9: Snapshot benchmark interpretation

> **Reviewer comment**: The snapshot benchmark speedup value (originally reported as a ratio) was misleading given the semantic non-comparability between OmicsLake's `snap()` and the file-copy proxy baseline.

**Response**: Table 6 now displays "---" in the snapshot speedup column instead of a numeric ratio. Footnote b explains: "Snapshot and file-copy proxy are not semantically identical; direct speedup ratio is omitted." Figure 3 uses "N/C" (not comparable) for the snapshot operation. The body text reports absolute timings only (OmicsLake `snap()`: 0.033 s; file-copy proxy: 0.008 s) and explicitly states that these ratios are operation-cost indicators under this specific benchmark configuration, not universal end-to-end speedup claims. The benchmark target is documented as one 1,000,000-row, 3-column numeric table (~24 MB payload) per iteration.

**Location**: Section 3.1 (Performance Benchmarks), Table 6, Figure 3

---

### Comment M10: Generalizability

> **Reviewer comment**: The generalizability discussion used vague language ("unusual data types") and did not specify concrete untested categories or risk layers.

**Response**: The generalizability section now specifies concrete untested data types: `dgCMatrix` (sparse matrices), `DelayedArray`, and HDF5-backed objects from the HDF5Array package, as well as platform-specific binary encodings. For cross-environment portability, specific risk layers are enumerated: DuckDB binary format compatibility across versions, Arrow C Data Interface ABI differences, and Parquet codec/library-version mismatches.

**Location**: Section 4.3 (Generalizability), Section 4.3 (Environmental Scope)

---

### Comment M11: Data Availability and persistent identifiers

> **Reviewer comment**: The Data Availability section lacked persistent identifiers (DOI, Code Ocean capsule) required for review.

**Response**: The Data Availability section was updated to provide review access via the versioned repository release and a Software Heritage identifier (SWHID) snapshot. Zenodo DOI and Code Ocean capsule are described as pending deposition records. The following commitment was added: "If editorial policy requires pre-review DOI availability, the authors will expedite deposition upon request."

**Location**: Section 8 (Data Availability)

---

### Comment M12: Figure caption consistency (confidence interval wording)

> **Reviewer comment**: Figure 3 and related benchmark figures used "95% confidence intervals" to describe run dispersion, which is statistically incorrect for empirical percentile intervals from repeated benchmark runs.

**Response**: All benchmark figure captions and table notes were updated to use "empirical 2.5th--97.5th percentile interval across repeated runs (n=30)." The term "confidence interval" is now correctly reserved for statistical intervals only (e.g., Clopper--Pearson CI for success rates in Section 3.2). Figure script subtitles, main text, and table captions were updated consistently.

**Location**: Section 3.1, Table 6 caption, Figure 3 caption

---

## Minor Items

### Comment m1: Embedded figure-title prefixes in R scripts

> **Reviewer comment**: Figure R scripts included "Figure X:" prefixes in plot titles, duplicating the LaTeX `\caption{}` numbering.

**Response**: The "Figure X:" prefix was removed from plot titles in four figure R scripts. All four figure PDFs were regenerated and copied to the submission bundle.

**Location**: `inst/paper/figures/figure2_lineage_workflow.R`, `inst/paper/figures/figure4_reproducibility_workflow.R`, `inst/paper/figures/figure5_reproducibility_comparison.R`, `inst/paper/figures/figure6_storage_efficiency.R`

---

### Comment m2: "Deterministic cleanup" wording

> **Reviewer comment**: The phrase "deterministic cleanup" implied deterministic garbage-collection timing, which is not guaranteed in R.

**Response**: The wording was changed from "The finalize() method provides deterministic cleanup" to "The finalize() method provides automatic cleanup when the Lake object is garbage-collected."

**Location**: Section 2.1 (Implementation, Design Choice: R6 Classes)

---

### Comment m3: lstset comment formatting

> **Reviewer comment**: A LaTeX `lstset` comment format issue was flagged.

**Response**: The lstset comment formatting was corrected in the manuscript source.

**Location**: `inst/paper/manuscript/main.tex` (preamble)

---

### Comment m4: API table terminology

> **Reviewer comment**: The Core API table terminology needed standardization for snapshot-related operations.

**Response**: The Core API table now uses consistent terminology: "snapshot label / relative reference" for version-related parameters. `save_as()` and `ol_commit()` were added to the Core API table with descriptions of their roles. API complexity notation was standardized to asymptotic notation (`O(n)` and `O(sum n_i)`).

**Location**: Section 2.2 (Core API), Table 2

---

### Comment m5: Practical Impact paragraph wording

> **Reviewer comment**: The Practical Impact paragraph implied specific quantitative claims that were not empirically supported.

**Response**: The paragraph was rewritten with qualitative phase-level characterization. Function-specific parenthetical descriptions were replaced with cleaner qualitative statements. The phrase "To quantify" was changed to "To illustrate." The auto-tracking scope description now avoids implying specific percentage coverage.

**Location**: Section 2.4 (Scope and Limitations of Automatic Tracking)

---

### Comment m6: Supplementary Figure S2

> **Reviewer comment**: A supplementary figure summarizing major results was requested.

**Response**: Supplementary Figure S2 (Major Results Dashboard) was added with a figure environment and descriptive text in Appendix A.3.

**Location**: Section A.3 (Supplementary Figure S2: Major Results Dashboard)

---

## Additional Clarifications

### Checksum/verify claims

Non-implemented checksum and cryptographic integrity verification claims were removed and replaced with the implemented state-equivalence comparison and diagnostic workflow description.

**Location**: Section 2.6 (State Consistency Verification and Diagnostics)

### ACM reproducibility alignment

Attribute-preservation wording was updated to "value/name equivalence with class/attribute normalization where applicable" to avoid over-strong claims.

**Location**: Section 4.5 (Alignment with ACM Reproducibility Definitions)

### `check.attributes = FALSE` rationale

An explicit rationale was added for the use of `check.attributes = FALSE` in RT-001: non-semantic backend/class metadata differences (e.g., DuckDB connection attributes) are excluded while value, name, and type equivalence remains enforced.

**Location**: Section 3.2.1 (RT-001)

### Git LFS and memoise citations

References for Git LFS and memoise were added to the bibliography and cited in the Introduction where these tools are discussed.

**Location**: Section 1 (Introduction)

### Additional join baselines

Supplementary Table S3 explicitly includes data.table, dplyr, direct DuckDB-backed path, and base::merge reference row as additional join baselines.

**Location**: Appendix (Supplementary Table S3)

---

## Manuscript Section Cross-Reference

For reviewer convenience, the following maps major revision themes to manuscript sections:

| Theme | Manuscript location |
|-------|-------------------|
| Lineage granularity (dataset-name level) | Section 2.1, Section 2.3 |
| Data model and storage semantics | Section 2.3 |
| Core API and put/snap/restore | Section 2.2, Section 2.3 |
| Automatic dependency tracking | Section 2.3, Section 2.4 |
| Performance benchmarks | Section 3.1, Table 6, Figure 3 |
| Reproducibility validation | Section 3.2, Table 9, Figures 4--5 |
| Simulated cross-environment transfer | Section 3.2.3 |
| Generalizability and risk layers | Section 4.3 |
| FAIR alignment | Section 4.4 |
| ACM reproducibility alignment | Section 4.5 |
| Floating-point tolerance | Section 3.2.1, Appendix A.4 |
| Storage layer roles | Abstract, Section 2.1, Figure 1 |
| Data Availability and PIDs | Section 8 |
| State consistency verification | Section 2.6 |
| Supplementary Figure S2 | Appendix A.3 |
