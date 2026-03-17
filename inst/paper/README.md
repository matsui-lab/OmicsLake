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
├── 04_breakage_taxonomy_validation.R  # Comprehensive reproducibility breakage taxonomy validation
├── 06_limit_stress_test.R          # Fairness-focused limit/boundary stress tests
├── 05_reproducibility_scorecard.R  # Unified scorecard (common + mode-specific axes)
├── reproducibility_breakage_taxonomy.json  # Breakage categories, causes, and expected resolution modes
├── reproducibility_limit_taxonomy.json     # Limit/boundary categories and fairness criteria
├── benchmark_datasets/             # Generated benchmark data (created by 00_generate_datasets.R)
│   ├── dataset_100MB.parquet       # 100MB Parquet dataset for I/O benchmarks
│   ├── rds_100MB.RDS               # 100MB RDS dataset for baseline comparison
│   ├── table1_1M.csv               # 1M row table for join benchmarks
│   ├── table2_1M.csv               # 1M row table for join benchmarks
│   └── rnaseq_mock_data.RData      # Mock RNA-seq data for reproducibility tests
├── results_performance.RDS         # Performance benchmark results (generated)
├── results_reproducibility_summary.csv   # RT-001..RT-005 summary metrics (generated)
├── results_breakage_coverage_summary.csv # Breakage taxonomy coverage metrics (generated)
├── results_limit_coverage_summary.csv    # Limit/boundary coverage metrics (generated)
├── results_unified_scorecard.csv   # Unified objective scorecard (generated)
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

# 4. Run comprehensive breakage taxonomy validation
Rscript 04_breakage_taxonomy_validation.R

# 5. Run fairness-focused limit/boundary stress tests
Rscript 06_limit_stress_test.R

# 6. Build unified evaluation scorecard
Rscript 05_reproducibility_scorecard.R
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
- `results_reproducibility_summary.csv` - RT-001 to RT-005 summary table
- `results_table3_comparison.csv` - Narrative comparison table
- `results_table3_quantitative.csv` - Quantitative reproducibility table
- `results_rt004_scalability.csv` - RT-004 detailed scalability data
- `results_reproducibility_detailed.RDS` - Full reproducibility report
- `figure_reproducibility_comparison.pdf` - Capability-level comparison figure
- `figure_scalability.pdf` - Scalability figure

**Expected Comparison Output:**
| Environment | Restoration/Tracking Summary | Evidence Type |
|-------------|------------------------------|---------------|
| Standard R Script | Ad hoc reconstruction, no automatic lineage | Qualitative baseline |
| Git + Manual Versioning | Manual conventions and checklist-based recovery | Qualitative baseline |
| OmicsLake | RT-001/002/003/005 observed at 100% in this protocol | Empirical (this study) |

#### 4. Breakage Taxonomy Validation (`04_breakage_taxonomy_validation.R`)

Systematically validates six reproducibility breakage categories:

1. Input drift
2. Schema drift
3. Parameter drift
4. Artifact loss
5. Git provenance gap
6. renv lockfile gap

For each category, the script:

- reproduces the failure mode in a controlled pipeline
- records conventional-process bottlenecks
- runs OmicsLake 5-step diagnosis (`lake_repair`)
- evaluates safe auto actions and restoration outcomes

**Output:**

- `results_breakage_evaluation.csv`
- `results_breakage_coverage_summary.csv`
- `results_breakage_mode_summary.csv`
- `results_breakage_report.md`

#### 5. Unified Scorecard (`05_reproducibility_scorecard.R`)

Integrates objective metrics from:

- RT-001 to RT-005 reproducibility tests (common axes)
- breakage taxonomy validation (auto rollback + guided manual axes)
- limit/boundary stress test (false positive/false negative/restore precondition limits; optional if `results_limit_coverage_summary.csv` exists)

**Output:**

- `results_unified_scorecard.csv`
- `results_unified_scope_summary.csv`
- `results_unified_scorecard.md`

#### 6. Limit/Boundary Stress Test (`06_limit_stress_test.R`)

Evaluates fairness-critical boundary conditions:

1. Semantic-equivalent row-order changes
2. Tiny numeric jitter below practical tolerance
3. Untracked external dependency drift
4. Rollback request without snapshot precondition
5. Stochastic recomputation without fixed seed

**Output:**

- `results_limit_evaluation.csv`
- `results_limit_coverage_summary.csv`
- `results_limit_confusion_matrix.csv`
- `results_limit_report.md`

## Publication-Ready Outputs

The reproducibility and breakage scripts generate manuscript-ready tables/figures and machine-readable scorecards:

### Table: RT-001 to RT-005 Summary
**File:** `results_reproducibility_summary.csv`

Includes pass/fail, confidence intervals, and targets for:
- RT-001 State restoration
- RT-002 Lineage tracking
- RT-003 Cross-environment reproducibility
- RT-004 Long-term stability
- RT-005 Rollback cascade

### Table: Comparative Narratives and Quantitative Values
**Files:**
- `results_table3_comparison.csv`
- `results_table3_quantitative.csv`

### Figure: Reproducibility Comparison
**File:** `figure_reproducibility_comparison.pdf`

### Figure: Scalability (RT-004)
**File:** `figure_scalability.pdf`

### Breakage Evaluation Outputs
**Files:**
- `results_breakage_evaluation.csv`
- `results_breakage_coverage_summary.csv`
- `results_breakage_mode_summary.csv`
- `results_breakage_report.md`

### Limit/Boundary Evaluation Outputs
**Files:**
- `results_limit_evaluation.csv`
- `results_limit_coverage_summary.csv`
- `results_limit_confusion_matrix.csv`
- `results_limit_report.md`

### Unified Evaluation Outputs
**Files:**
- `results_unified_scorecard.csv`
- `results_unified_scope_summary.csv`
- `results_unified_scorecard.md`

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
# Load RT summary
summary <- read.csv("results_reproducibility_summary.csv")
print(summary)

# Load detailed report
report <- readRDS("results_reproducibility_detailed.RDS")

# Inspect RT-001 details
print(report$rt001)

# Load breakage summary
breakage <- read.csv("results_breakage_coverage_summary.csv")
print(breakage)

# Load limit/boundary summary
limits <- read.csv("results_limit_coverage_summary.csv")
print(limits)

# Load unified scorecard
scorecard <- read.csv("results_unified_scorecard.csv")
print(scorecard)
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
