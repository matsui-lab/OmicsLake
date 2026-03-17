# OmicsLake Peer Review Report
## Reviewer 1: Bioinformatics Expert

**Manuscript:** OmicsLake: Versioned Data Management with Automatic Lineage Tracking for Reproducible Bioinformatics

**Journal:** GigaScience (Technical Note)

**Date:** 2026-02-12

---

## Summary

This manuscript presents OmicsLake, an R package for versioned data management with automatic lineage tracking in bioinformatics workflows. The authors address a genuine gap in the reproducibility toolkit by focusing on data state reproducibility during interactive analysis sessions. The implementation leverages DuckDB, Apache Arrow, and Parquet for efficient storage and query processing.

---

## Major Concerns

### MC1: SummarizedExperiment Integration Claims Lack Depth

**Lines 403-436**

The manuscript claims "native support for Bioconductor's core data structures" and describes automatic decomposition of SummarizedExperiment objects. However, several critical aspects are inadequately addressed:

1. **Sparse matrix handling**: The claim that "sparse matrix support via the Matrix package" achieves "substantial storage reduction" (line 436) lacks quantitative evidence. Single-cell datasets with 90-95% sparsity would benefit enormously from proper sparse storage, but no benchmarks demonstrate the actual compression ratio compared to dense serialization.

2. **rowRanges preservation**: The manuscript does not mention `RangedSummarizedExperiment` or how genomic coordinates (`GRanges`) are preserved. Bioinformaticians rely heavily on coordinate-based operations, and loss of range information would be a critical limitation.

3. **Missing SingleCellExperiment support**: The manuscript mentions single-cell workflows but does not address `SingleCellExperiment`, which extends `SummarizedExperiment` with essential fields (`reducedDims`, `altExps`, `colPairs`, `rowPairs`). This is a significant omission for the claimed single-cell use case.

**Recommendation:** Provide empirical benchmarks for sparse matrix storage efficiency. Clarify support for `RangedSummarizedExperiment` and `SingleCellExperiment`. If these are not currently supported, explicitly state these as limitations.

### MC2: RNA-seq Workflow Description is Superficial

**Lines 96-97, 470-478**

The manuscript references "a typical RNA-seq analysis pipeline" involving "quality control, read alignment, quantification, normalization, batch correction, differential expression analysis, and pathway enrichment" but provides only a high-level five-step validation workflow (RT-001). This simplification misrepresents the complexity of real bioinformatics analyses:

1. **Missing critical steps**: No mention of count normalization methods (DESeq2's median-of-ratios, edgeR's TMM, etc.), batch effect correction (ComBat, limma's removeBatchEffect), or multiple testing correction considerations.

2. **No multi-assay integration**: Modern omics studies frequently involve multi-omics integration. While `MultiAssayExperiment` support is mentioned in `bioc.R`, this is absent from the manuscript.

3. **Parameter sensitivity**: Different normalization or filtering thresholds can dramatically alter differential expression results. The manuscript should demonstrate how OmicsLake captures these parameter dependencies.

**Recommendation:** Include a realistic RNA-seq workflow example with actual Bioconductor packages (DESeq2, edgeR, limma-voom). Show lineage tracking through normalization and differential expression steps with specific parameter logging.

### MC3: Single-Cell Analysis Claims are Unsubstantiated

**Lines 96, 436-437**

The abstract and introduction reference single-cell RNA-seq, but the manuscript provides no concrete single-cell examples or benchmarks. Specific concerns:

1. **Scale validation**: Single-cell datasets routinely contain 10,000-100,000+ cells with 20,000+ genes. The manuscript's benchmarks use only "1M rows" which may not reflect the memory and performance characteristics of real single-cell matrices.

2. **Seurat/Bioconductor integration**: The implementation shows `Seurat` type detection but uses fallback serialization (`ol_save`). No adapter exists for proper Seurat object decomposition, unlike SummarizedExperiment.

3. **scRNA-seq-specific operations**: Common operations like dimensionality reduction (PCA, UMAP, t-SNE), clustering, and marker gene identification are not addressed.

**Recommendation:** Either provide substantive single-cell analysis examples with realistic dataset sizes (e.g., 10x Genomics PBMC dataset), or remove single-cell claims from the abstract and introduction.

### MC4: Incomplete Comparison with Workflow Managers

**Table 1, Lines 100-105**

The comparison with Snakemake and Nextflow is misleading:

1. **"Interactive session support: No"**: This oversimplifies. Snakemake can be invoked from interactive sessions, and Nextflow Tower provides interactive monitoring. The distinction should be about workflow definition requirements, not interactivity.

2. **"Automatic lineage tracking: Yes*"**: The asterisk note ("Requires predefined workflow specification") applies to OmicsLake's own limitations since it only tracks dplyr operations automatically. Non-dplyr operations also require explicit specification.

3. **Missing provenance depth**: Snakemake and Nextflow track not just data lineage but also computational resources, execution times, and command-line parameters. OmicsLake's lineage is comparatively shallow.

4. **Complementary nature understated**: The manuscript mentions in Discussion that OmicsLake can "export to/import from" workflow managers (line 639), but this integration is not demonstrated.

**Recommendation:** Revise Table 1 to provide a more nuanced comparison. Add a row for "Provenance metadata depth" or "Computational parameter tracking". Show concrete examples of OmicsLake-Snakemake integration.

---

## Minor Concerns

### mc1: Bioconductor Package Versioning Not Addressed

The manuscript discusses reproducibility extensively but does not address Bioconductor's biannual release cycle and package versioning. Data created with SummarizedExperiment 1.28 may not deserialize correctly with version 1.30. How does OmicsLake handle cross-version compatibility?

**Recommendation:** Add a statement about recommended practices for environment reproducibility (e.g., using renv alongside OmicsLake).

### mc2: Missing GEO/ArrayExpress Integration

Bioinformaticians frequently download data from public repositories (GEO, ArrayExpress, SRA). An `ol_import_geo()` or similar function would significantly enhance usability.

**Recommendation:** Mention potential integration with public data repositories as future work.

### mc3: DESeq2/edgeR Object Support Unclear

The manuscript does not mention support for `DESeqDataSet` or `DGEList` objects, which are central to differential expression workflows. These inherit from or work with SummarizedExperiment but have additional components.

**Recommendation:** Clarify whether these objects are supported, and if so, how their specific components (dispersion estimates, contrast matrices) are preserved.

### mc4: Lineage Tracking Scope Needs Explicit Examples

**Lines 395-401**

The limitations of automatic tracking are mentioned but not illustrated. Concrete examples would help users understand when manual annotation is required:

- DESeq2: `dds <- DESeq(dds)` - Not tracked
- Seurat: `obj <- NormalizeData(obj)` - Not tracked
- Base R: `mat <- log2(mat + 1)` - Not tracked

**Recommendation:** Add a table or code block showing common operations and their tracking status.

### mc5: Terminology Inconsistency

The manuscript uses both "snapshot" and "version" with subtle distinctions (lines 206-209), but this terminology differs from common bioinformatics practice where "version" typically refers to software versions. Consider aligning with established provenance terminology (e.g., W3C PROV model).

### mc6: Memory Constraints Underestimated

**Line 621**

The manuscript notes that `put()` "requires the full dataset to fit in memory" and suggests chunked processing. However, this significantly limits utility for large single-cell datasets or whole-genome bisulfite sequencing data.

**Recommendation:** Discuss integration with DelayedArray/HDF5Array for out-of-core processing as future work.

### mc7: Missing Citation for Sparse Matrix Efficiency

**Line 436**

The claim of "90-95% zero values being typical" for scRNA-seq should be supported by a citation (e.g., from 10x Genomics documentation or relevant publications).

---

## Strengths

### S1: Addresses a Real Gap

The manuscript correctly identifies that code versioning (Git) and environment versioning (Docker/renv) are well-established, but data state versioning remains poorly addressed. This is a genuine pain point in bioinformatics practice.

### S2: Thoughtful API Design

The fluent API (`lake$put()`, `lake$get()`, `lake$snap()`, `lake$tree()`) and dplyr integration demonstrate user-centered design. The `save_as()` function at the end of dplyr pipelines is elegant.

### S3: Rigorous Validation Framework

The reproducibility test protocol (RT-001 through RT-005) is well-designed with clear success criteria. The use of `all.equal()` with explicit tolerance parameters demonstrates attention to numerical reproducibility.

### S4: FAIR Alignment

The explicit mapping to FAIR principles (Section 4.3) is valuable for researchers navigating funding agency requirements for data management.

### S5: Technical Architecture is Sound

The combination of DuckDB (query engine), Arrow (zero-copy transfer), and Parquet (columnar storage) represents modern best practices for analytical data management.

### S6: Honest Limitation Disclosure

The authors appropriately acknowledge that automatic lineage tracking is limited to dplyr operations and that memory constraints exist for very large datasets.

---

## Recommendations

### R1: Add Realistic Bioinformatics Workflow Example

Include a complete, reproducible example using actual Bioconductor packages. Suggested workflow:

```r
# Load RNA-seq counts from GEO (example)
lake <- Lake$new("rnaseq_analysis")
lake$put("raw_counts", counts_matrix)

# DESeq2 workflow with explicit dependency tracking
library(DESeq2)
dds <- DESeqDataSetFromMatrix(
  countData = lake$get("raw_counts"),
  colData = lake$get("sample_metadata"),
  design = ~ condition
)
dds <- DESeq(dds)
lake$put("deseq_results", dds, depends_on = c("raw_counts", "sample_metadata"))

# Snapshot at publication
lake$snap("v1.0_publication")
```

### R2: Benchmark with Realistic Single-Cell Data

If single-cell claims are retained, include benchmarks with a standard dataset (e.g., PBMC 10k cells):
- Storage efficiency: OmicsLake vs. RDS vs. H5AD
- Query performance: Subsetting cells by cluster
- Round-trip fidelity: Store and retrieve SingleCellExperiment

### R3: Clarify Scope More Explicitly

Add a "Scope and Limitations" subsection in the Introduction that clearly states:
- Supported Bioconductor classes (with version requirements)
- Operations that require manual lineage annotation
- Recommended companion tools (renv, Git, Docker)

### R4: Demonstrate Workflow Manager Integration

Add supplementary material showing integration with Snakemake:

```python
# Snakemake rule that reads from OmicsLake
rule differential_expression:
    input:
        counts = "lake://rnaseq_project/normalized_counts@v1.0"
    output:
        results = "lake://rnaseq_project/de_results"
```

### R5: Consider Bioconductor Submission

Given the focus on Bioconductor integration, the package would benefit from Bioconductor submission. This would ensure:
- Compatibility testing across Bioconductor releases
- Integration with BiocManager installation
- Visibility to the target audience

---

## Decision Recommendation

**Major Revision Required**

The manuscript addresses a valuable problem and presents a technically sound solution, but the bioinformatics-specific claims require substantiation. The primary concerns are:

1. Single-cell analysis claims lack evidence (MC3)
2. RNA-seq workflow description is oversimplified (MC2)
3. SummarizedExperiment support needs clarification (MC1)
4. Workflow manager comparison needs revision (MC4)

After addressing these concerns, the manuscript would represent a valuable contribution to the bioinformatics reproducibility toolkit.

---

*Reviewer 1*
*Bioinformatics Expert*
