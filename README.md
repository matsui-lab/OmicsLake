# OmicsLake

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**OmicsLake** is a comprehensive R package for versioned, reproducible omics data management and analysis. Built on DuckDB, Apache Arrow, and Parquet, it provides a powerful framework for tracking data provenance, managing analysis dependencies, and ensuring computational reproducibility in bioinformatics workflows.

## 🌟 Key Features

### 📦 **Project-Based Data Management**
- **Isolated Projects**: Each analysis project has its own DuckDB database and versioned workspace
- **Tables & R Objects**: Store both tabular data and arbitrary R objects with full version control
- **Efficient Storage**: Leverage Parquet compression and DuckDB's columnar storage for large datasets

### 🔄 **Comprehensive Version Control**
- **Automatic Snapshots**: Every write operation creates a timestamped snapshot
- **Tagging & Labels**: Apply human-readable tags to specific versions and project-wide labels
- **Version Comparison**: Compare object versions across different analysis parameters and methods
- **Time Travel**: Access any historical version using tags, timestamps, or relative references

### 🔗 **Dependency Tracking**
- **Automatic Lineage**: Track dependencies between tables, objects, and analysis steps
- **Provenance Graphs**: Visualize complete data lineage with interactive dependency graphs
- **Impact Analysis**: Identify downstream effects when updating upstream data

### 🚀 **Advanced Analytics**
- **SQL Queries**: Full DuckDB SQL support with JOINs, subqueries, and window functions
- **Aggregation Functions**: Built-in functions for common statistical operations
- **Lazy Evaluation**: Integrate seamlessly with dplyr for efficient query composition
- **Database Views**: Create reusable views for complex queries and multi-version comparisons

### 📊 **High-Performance Data Exchange**
- **Parquet Import/Export**: Efficient columnar storage with multiple compression options
- **Cloud-Ready**: Export to Parquet for sharing via cloud storage or collaboration
- **Cross-Platform**: Share data with Python, Spark, and other Parquet-compatible tools

### 🧬 **Bioconductor Integration**
- **SummarizedExperiment**: Direct conversion to/from Bioconductor's standard container
- **MultiAssayExperiment**: Support for multi-omics experiments
- **Memory/HDF5 Backing**: Flexible storage backends for different use cases

## 💡 Main Use Cases

### 1. **Reproducible Differential Expression Analysis**
Track the complete provenance of your RNA-seq analysis from raw counts to final results:

```r
library(OmicsLake)

# Initialize project
ol_init("rnaseq_analysis")

# Store raw data with automatic versioning
ol_write("raw_counts", counts_df)
ol_commit("Import raw count matrix")
ol_label("v1_raw_data")

# Save normalization parameters
norm_params <- list(method = "TMM", log_transform = TRUE)
ol_save("norm_params", norm_params)

# Save normalized data with dependency tracking
ol_write("normalized_counts", norm_df, 
         depends_on = c("raw_counts", "norm_params"))

# Perform DE analysis with multiple methods
ol_save("de_deseq2", deseq2_results, 
        depends_on = c("normalized_counts", "sample_metadata"))
ol_tag_object("de_deseq2", "deseq2_strict")

ol_save("de_edger", edger_results,
        depends_on = c("normalized_counts", "sample_metadata"))
ol_tag_object("de_edger", "edger_default")

# Compare results across methods using views
ol_create_view("method_comparison",
  "SELECT d.gene_id, d.log2fc AS deseq2_lfc, e.log2fc AS edger_lfc,
          d.padj AS deseq2_padj, e.padj AS edger_padj
   FROM de_deseq2 d
   INNER JOIN de_edger e ON d.gene_id = e.gene_id",
  depends_on = c("de_deseq2", "de_edger")
)

# Visualize complete analysis lineage
ol_plot_lineage("de_deseq2", direction = "upstream")
```

### 2. **Parameter Sweep & Method Comparison**
Systematically explore analysis parameters and compare results:

```r
# Test different p-value thresholds
for (alpha in c(0.01, 0.05, 0.10)) {
  results <- run_de_analysis(data, alpha = alpha)
  ol_save("de_results", results)
  ol_tag_object("de_results", paste0("alpha_", alpha))
}

# Compare all versions
comparison <- ol_compare_versions("de_results")
print(comparison)  # Shows size changes, dependencies, timing

# Load specific version for downstream analysis
strict_results <- ol_read_object("de_results", ref = "@tag(alpha_0.01)")
```

### 3. **Collaborative Data Sharing**
Share analysis results efficiently with collaborators:

```r
# Export to Parquet with optimal compression
ol_export_parquet("de_results", "results.parquet",
                  compression = "zstd",
                  compression_level = 3)

# Collaborator imports and continues analysis
ol_import_parquet("external_qc_data.parquet", "qc_filtered",
                  depends_on = "raw_counts",
                  mode = "create")
```

### 4. **Quality Control Checkpoints**
Create labeled checkpoints at each QC stage:

```r
ol_write("raw_data", raw)
ol_label("stage1_raw")

ol_write("qc_filtered", filtered_data)
ol_label("stage2_qc_passed")

ol_write("normalized", normalized_data)
ol_label("stage3_ready_for_analysis")

# Rollback to any stage
ol_checkout("stage2_qc_passed")
```

## 📥 Installation

### Prerequisites
- R (≥ 4.3)
- C++ compiler (for DuckDB package compilation, if needed)

### Install Dependencies

```r
install.packages(c(
  "arrow",      # Apache Arrow for efficient data handling
  "duckdb",     # DuckDB for analytical database
  "DBI",        # Database interface
  "dplyr",      # Data manipulation (integrates with DuckDB)
  "dbplyr",     # dplyr backend for databases
  "jsonlite",   # JSON handling for metadata
  "digest"      # Hashing for version identification
))

# Optional: Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("SummarizedExperiment", "S4Vectors"))
```

### Install OmicsLake

```r
# From GitHub (recommended for latest version)
install.packages("remotes")
remotes::install_github("matsui-lab/OmicsLake")

# For development
git clone https://github.com/matsui-lab/OmicsLake.git
cd OmicsLake
R -e "devtools::install()"
```

## 🚀 Quick Start

```r
library(OmicsLake)

# Initialize a project
ol_init("my_analysis")

# Write data to a table
df <- data.frame(gene_id = paste0("GENE", 1:100),
                 expression = rnorm(100, 50, 10))
ol_write("expression_data", df)

# Commit with a message
snapshot_id <- ol_commit("Initial data import")

# Create a label for this state
ol_label("baseline")

# Read data back
data <- ol_read("expression_data")

# Query with SQL
high_expr <- ol_query(
  "SELECT * FROM expression_data WHERE expression > 55"
)

# Save R objects with version control
params <- list(threshold = 55, method = "standard")
ol_save("analysis_params", params)

# View commit history
ol_log("expression_data")

# Visualize dependencies (requires igraph)
ol_plot_lineage("analysis_params", direction = "both")
```

## 📚 Documentation

### Vignettes
- **[Comprehensive Guide](vignettes/omicslake_comprehensive_guide.Rmd)**: Complete reference for all features
- **[Practical Workflow](vignettes/omicslake_practical_workflow.Rmd)**: Real-world RNA-seq analysis example
- **[User Guide](vignettes/omicslake_user_guide.md)**: Getting started tutorial

### Key Functions

#### Project Management
- `ol_init()` - Initialize a project
- `ol_label()` - Create project-wide labels
- `ol_checkout()` - Restore to a labeled state
- `ol_commit()` - Create commits with messages

#### Data I/O
- `ol_write()` - Save tables
- `ol_read()` - Load tables
- `ol_save()` - Save R objects
- `ol_read_object()` - Load R objects
- `ol_query()` - Execute SQL queries

#### Version Control
- `ol_tag()` / `ol_tag_object()` - Tag specific versions
- `ol_list_object_versions()` - List all versions
- `ol_compare_versions()` - Compare version differences
- `ol_log()` - View table history

#### Analytics
- `ol_aggregate()` - Aggregate with grouping
- `ol_add_rank()` - Add ranking columns
- `ol_top_n()` - Get top N records
- `ol_moving_avg()` - Calculate moving averages
- `ol_cumulative_sum()` - Compute cumulative sums

#### Data Exchange
- `ol_export_parquet()` - Export to Parquet format
- `ol_import_parquet()` - Import from Parquet files

#### Views
- `ol_create_view()` - Create database views
- `ol_list_views()` - List all views
- `ol_drop_view()` - Remove views

#### Dependencies
- `ol_get_dependencies()` - Get direct dependencies
- `ol_show_lineage()` - Show complete lineage tree
- `ol_plot_lineage()` - Visualize dependency graph

## 🏗️ Architecture

OmicsLake combines several powerful technologies:

- **DuckDB**: In-process analytical database for fast SQL queries
- **Apache Arrow**: Zero-copy data sharing and efficient memory management
- **Parquet**: Columnar storage format for compression and performance
- **R**: Statistical computing and bioinformatics ecosystem

The package maintains project state in a DuckDB database with automatic snapshot creation, dependency tracking tables, and metadata management.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## 📄 License

MIT License. See [LICENSE](LICENSE) file for details.

## 👥 Authors

- **Yusuke Matsui** - Package author and maintainer

## 🔗 Links

- **GitHub Repository**: [https://github.com/matsui-lab/OmicsLake](https://github.com/matsui-lab/OmicsLake)
- **Issue Tracker**: [https://github.com/matsui-lab/OmicsLake/issues](https://github.com/matsui-lab/OmicsLake/issues)

## 📖 Citation

If you use OmicsLake in your research, please cite:

```
Matsui, Y. (2024). OmicsLake: Versioned, Reproducible Omics Data Management for R.
GitHub repository: https://github.com/matsui-lab/OmicsLake
```
