# OmicsLake Paper: Performance and Reproducibility Benchmarks

This directory contains benchmark scripts and datasets for validating the performance and reproducibility claims in the OmicsLake paper's *Results and Evaluation* section.

## Overview

OmicsLake is a comprehensive version control and dependency tracking system for bioinformatics data analysis, built on:
- **DuckDB**: In-process analytical database for fast SQL queries
- **Apache Arrow**: Zero-copy data sharing and efficient memory management
- **Parquet**: Columnar storage format for optimized compression and performance

## Directory Structure

```
inst/paper/
├── 00_generate_datasets.R          # Generate synthetic benchmark datasets
├── 01_performance_benchmark.R      # Performance benchmarks (I/O, aggregation, joins, snapshots)
├── 02_reproducibility_test.R       # Reproducibility validation for multi-step workflows
├── benchmark_datasets/             # Generated benchmark data (created by 00_generate_datasets.R)
│   ├── dataset_100MB.parquet       # 100MB Parquet dataset for I/O benchmarks
│   ├── rds_100MB.RDS               # 100MB RDS dataset for baseline comparison
│   ├── table1_1M.csv               # 1M row table for join benchmarks
│   ├── table2_1M.csv               # 1M row table for join benchmarks
│   └── rnaseq_mock_data.RData      # Mock RNA-seq data for reproducibility tests
├── results_performance.RDS         # Performance benchmark results (generated)
├── results_reproducibility.csv     # Reproducibility metrics (generated)
└── README.md                       # This file
```

## System Requirements

### Hardware
- **OS:** Ubuntu 24.04 LTS (or compatible Linux distribution)
- **CPU:** AMD Ryzen 9 7950X or equivalent (16+ cores recommended)
- **Memory:** ≥128 GB RAM (for large-scale benchmarks)
- **Disk:** ≥10 GB free space for benchmark datasets and results

### Software
- **R version:** ≥ 4.3.1
- **Required R packages:**
  ```r
  install.packages(c("duckdb", "arrow", "DBI", "dplyr", "bench", "jsonlite", "digest"))
  
  # Install Bioconductor packages
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install(c("SummarizedExperiment", "S4Vectors"))
  
  # Install OmicsLake (from source or CRAN)
  devtools::install_github("matsui-lab/OmicsLake")
  # or: install.packages("OmicsLake")
  ```

## Running the Benchmarks

### Quick Start

Run all benchmarks in sequence:

```bash
cd inst/paper

# 1. Generate benchmark datasets
Rscript 00_generate_datasets.R

# 2. Run performance benchmarks
Rscript 01_performance_benchmark.R

# 3. Run reproducibility tests
Rscript 02_reproducibility_test.R
```

### Individual Benchmark Scripts

#### 1. Dataset Generation (`00_generate_datasets.R`)

Generates synthetic datasets for benchmarking:
- 100MB Parquet/RDS datasets (1M rows, 10 columns)
- 1M row CSV tables for join operations
- Mock RNA-seq count data (20K genes × 6 samples)

**Runtime:** ~2-5 minutes  
**Output:** `benchmark_datasets/` directory with all required files

#### 2. Performance Benchmark (`01_performance_benchmark.R`)

Quantitatively compares OmicsLake against base R and SQLite workflows across four key operations:

**Benchmarks:**
1. **Table Import:** Parquet (OmicsLake) vs RDS (base R) loading
2. **Aggregation:** DuckDB SQL vs dplyr on 1M rows with grouping
3. **Join:** DuckDB SQL vs base `merge()` on 1M × 1M rows
4. **Snapshot:** Metadata-based commit vs file copy
5. **Storage Efficiency:** Parquet vs RDS compression

**Runtime:** ~10-20 minutes (depending on hardware)  
**Output:** `results_performance.RDS` with detailed benchmark results

**Expected Performance Gains:**
| Task | Expected Speedup | Metric |
|------|-----------------|--------|
| Table Import | 3-4× faster | Load time |
| Aggregation | 2-3× faster | CPU time |
| Join | 2-3× faster | Execution time + 60-70% memory reduction |
| Snapshot | 10-100× faster | Disk I/O (metadata vs full copy) |
| Storage | 40-60% smaller | Disk usage (Parquet compression) |

#### 3. Reproducibility Test (`02_reproducibility_test.R`)

Validates OmicsLake's reproducibility guarantees in a multi-step RNA-seq analysis workflow:

**Workflow Steps:**
1. Import raw count data
2. Normalization (log2 transformation)
3. Quality control filtering
4. Differential expression analysis
5. Pathway enrichment (mock)

**Tests:**
- Exact data reproduction from version labels
- Dependency lineage tracking
- Rollback and state restoration
- Comparison with non-versioned approaches

**Runtime:** ~5-10 minutes  
**Output:** 
- `results_reproducibility_table.csv` - Comparison metrics (Table 1 for paper)
- `results_reproducibility_detailed.RDS` - Full reproducibility report
- `figure4_reproducibility_workflow.mmd` - Workflow diagram (Figure 4, Mermaid format)
- `figure5_reproducibility_metrics.pdf` - Quantitative metrics plot (Figure 5)
- `figure5_metrics_data.csv` - Metrics data for Figure 5

**Expected Reproducibility Rates:**
| Environment | Steps Reproduced | Reproducibility % |
|-------------|------------------|-------------------|
| Standard R Script | 4/10 | 40% |
| Git + Manual Versioning | 7/10 | 70% |
| OmicsLake | 10/10 | 100% |

## Publication-Ready Outputs

The reproducibility test script automatically generates figures and tables ready for manuscript inclusion:

### Table 1: Reproducibility Comparison Across Environments
**File:** `results_reproducibility_table.csv`

Compares OmicsLake against traditional approaches across multiple dimensions:
- Steps successfully reproduced (out of 10 total)
- Data integrity guarantees
- Dependency tracking capabilities
- Rollback/restoration support
- Overall reproducibility percentage

### Figure 4: OmicsLake Reproducibility Workflow
**File:** `figure4_reproducibility_workflow.mmd` (Mermaid diagram)

Visual representation of the 5-step RNA-seq workflow demonstrating:
- Sequential analysis steps (Import → Normalization → QC → DE → Enrichment)
- Version label creation with `ol_label()`
- Rollback testing with `ol_read()` using `ref` parameter
- Data integrity validation with `all.equal()` (tolerance = 1e-8)

**Rendering:** Convert to SVG/PNG using:
```bash
# Using mermaid-cli
mmdc -i figure4_reproducibility_workflow.mmd -o figure4_reproducibility_workflow.svg

# Or use online tools: https://mermaid.live/
```

### Figure 5: Quantitative Reproducibility Metrics
**Files:** 
- `figure5_reproducibility_metrics.pdf` (requires ggplot2)
- `figure5_metrics_data.csv` (raw data)

Faceted bar plot comparing three environments across four key metrics:
- **Steps Reproduced:** Number of workflow steps successfully reproduced (out of 10)
- **Dependency Tracking:** Percentage of dependencies automatically tracked
- **Reproduction Accuracy:** Data integrity validation success rate
- **Human Overhead:** Time investment required per analysis (hours)

**Note:** PDF generation requires ggplot2. Install with: `install.packages("ggplot2")`

## Interpreting Results

### Performance Benchmark Results

The `results_performance.RDS` file contains a list with the following components:

```r
results <- readRDS("results_performance.RDS")

# View import benchmark
print(results$import)

# View aggregation benchmark
print(results$aggregation)

# View join benchmark
print(results$join)

# View snapshot benchmark
print(results$snapshot)

# View storage comparison
print(results$storage)
```

Key metrics from `bench::mark()`:
- `median`: Median execution time (most stable metric)
- `mem_alloc`: Peak memory allocation
- `n_itr`: Number of iterations performed
- `n_gc`: Number of garbage collections

### Reproducibility Test Results

```r
# Load reproducibility metrics
metrics <- read.csv("results_reproducibility.csv")
print(metrics)

# Load detailed report
report <- readRDS("results_reproducibility_detailed.RDS")

# Check if reproducibility test passed
print(report$test_passed)  # Should be TRUE

# View dependency tracking
print(report$dependencies)

# View commit history
print(report$commit_history)
```

## Troubleshooting

### Common Issues

**Issue:** "Benchmark datasets not found"  
**Solution:** Run `00_generate_datasets.R` first to create required datasets

**Issue:** Out of memory errors during join benchmark  
**Solution:** Reduce dataset size in `00_generate_datasets.R` (e.g., use 500K rows instead of 1M)

**Issue:** DuckDB connection errors  
**Solution:** Ensure DuckDB package is properly installed: `install.packages("duckdb")`

**Issue:** Arrow/Parquet errors  
**Solution:** Reinstall arrow package: `install.packages("arrow")`

### Performance Variability

Benchmark results may vary based on:
- System load and background processes
- Available RAM and CPU cores
- Disk I/O speed (SSD vs HDD)
- R version and package versions

For publication-quality results:
- Close unnecessary applications
- Run benchmarks multiple times and report median values
- Use dedicated hardware without background tasks
- Document exact system specifications

## Citation

If you use these benchmarks in your research, please cite:

```bibtex
@article{omicslake2024,
  title={OmicsLake: Versioned, On-Disk Omics Data Management for Bioconductor},
  author={Matsui, Yusuke and others},
  journal={TBD},
  year={2024}
}
```

## Contact

For questions or issues with the benchmarks:
- GitHub Issues: https://github.com/matsui-lab/OmicsLake/issues
- Email: you@example.org

## License

MIT License - See LICENSE file in the package root directory.
