# RRID Application Information for OmicsLake

## Resource Information

- **Resource Name**: OmicsLake
- **Resource Type**: Software/Tool
- **Description**: R package for versioned data management with automatic lineage tracking for reproducible bioinformatics workflows
- **Version**: 2.0.0
- **URL**: https://github.com/matsui-lab/OmicsLake
- **License**: MIT

## Vendor/Developer Information

- **Developer**: Matsui Lab, Nagasaki University
- **Contact**: ymatsui@nagasaki-u.ac.jp
- **Country**: Japan

## Technical Specifications

- **Programming Language**: R (>= 4.3)
- **Dependencies**: DuckDB, Apache Arrow, dplyr, R6
- **Platform**: Cross-platform (Windows, macOS, Linux)
- **Installation**: install.packages("OmicsLake") or devtools::install_github("matsui-lab/OmicsLake")

## Detailed Description for RRID Registry

OmicsLake is an R6 class-based package that provides automatic data versioning and lineage tracking for bioinformatics workflows. The architecture leverages three complementary technologies:

1. **DuckDB**: Embedded analytical query processing with O(n) sequential scan and O(n log n) indexed lookups
2. **Apache Arrow**: Zero-copy data interchange between R and the database layer
3. **Parquet**: Columnar storage with Snappy/Zstd compression achieving 40-60% size reduction over RDS format

### Key Features

- Fluent API (`lake$put()`, `lake$get()`, `lake$snap()`, `lake$tree()`) requiring no workflow restructuring
- S3 method dispatch on custom `lake_tbl` class for automatic lineage propagation through dplyr pipelines
- 100% state reproducibility for labeled snapshots
- Simulated cross-environment transfer validation with complete data portability in tested logical pairs
- FAIR-aligned provenance tracking

### Use Cases

- RNA-seq analysis workflow reproducibility
- Multi-omics data integration with lineage tracking
- Collaborative analysis with version control
- Manuscript revision support with checkpoint restoration

## Keywords

reproducibility, data lineage, version control, bioinformatics, R package, DuckDB, Parquet, FAIR data, data management, Bioconductor

## Related Publications

- [Manuscript in preparation for GigaScience]

## Application Steps

1. Go to https://scicrunch.org/resources
2. Click "Add a Resource"
3. Select "Software" as resource type
4. Fill in the information above
5. Provide detailed description and keywords
6. Submit for review
7. Respond to any curator questions
8. Once approved, update main.tex with assigned RRID

## Expected RRID Format

RRID:SCR_XXXXXX

## Post-Approval Checklist

After receiving the RRID, update the following files:

### 1. main.tex (Line 755)
```latex
\item[RRID:] SCR_XXXXXX
```

### 2. DESCRIPTION file
Add RRID to the package description or URL field.

### 3. README.md
Add RRID badge or reference in the package documentation.

### 4. Package vignettes
Reference the RRID in any published documentation.

## Notes

- RRID assignment typically takes 1-2 weeks
- The RRID is permanent and should be cited in all publications using OmicsLake
- Update the RRID in the manuscript before final submission
- SciCrunch curators may request additional information about the software
