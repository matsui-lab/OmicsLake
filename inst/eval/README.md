# OmicsLake Evaluation Suite

This directory contains the evaluation suite for OmicsLake v2.0, designed for paper and release benchmarking.

## Overview

The evaluation suite measures and validates OmicsLake's key claims:

- **C1 Overhead**: Lineage/versioning time and memory overhead
- **C2 Storage**: Tag/snap storage growth
- **C3 Pushdown**: SQL pushdown effectiveness with lazy evaluation
- **C4 Reproducibility**: Version-aware dataset lineage correctness

## Quick Start

```r
# Run full evaluation with default config
Rscript inst/eval/scripts/run_eval.R

# Run with specific config
Rscript inst/eval/scripts/run_eval.R --config inst/eval/configs/eval_small.yml

# Run benchmarks only
Rscript inst/eval/scripts/run_benchmarks.R --config inst/eval/configs/eval_medium.yml

# Run case study only
Rscript inst/eval/scripts/run_case_study.R

# Generate plots from existing results
Rscript inst/eval/scripts/plot_results.R inst/eval/results/benchmark_*.jsonl
```

## Configuration

Configuration files are in `configs/`:

| File | Description | Runtime |
|------|-------------|---------|
| `eval_small.yml` | Quick development testing | ~5 min |
| `eval_medium.yml` | Paper-grade evaluation | ~30-60 min |
| `eval_large.yml` | Comprehensive (high-memory) | ~2+ hours |
| `eval_default.yml` | Full documentation of options | varies |

### Key Configuration Options

```yaml
project_root: "~/.omicslake_eval"  # Evaluation data location
seed: 42                            # Random seed for reproducibility
threads: 4                          # DuckDB threads

sizes:
  small:
    n_rows: 100000
    n_cols: 20

reps:
  bench: 10    # Repetitions for timing measurements
  heavy: 5     # Repetitions for expensive operations

workloads:
  W0_io: true           # I/O benchmarks
  W1_queries: true      # Query benchmarks
  W2_lineage: true      # Lineage operations
  W3_case_study: true   # RNA-seq case study

baselines:
  B1_duckdb_dbplyr: true  # Raw DuckDB comparison
  B2_file_based: true     # File-based workflow comparison
```

## Workloads

### W0: I/O and Versioning

- **W0-1**: `put(table)` - Table write performance
- **W0-2**: `put(object)` - Object serialization performance
- **W0-3**: `tag/snap` - Versioning overhead and storage delta

### W1: Query Benchmarks

- **W1-1**: `get(where, select, collect=FALSE)` - Lazy query with pushdown
- **W1-2**: `ref() + dplyr` - Pipeline pushdown verification
- **W1-3**: Multi-parent joins (2 and 3 tables)

### W2: Lineage Operations

- **W2-1**: `deps/tree` - Dependency traversal scaling
- **W2-2**: `impact` - Downstream analysis

### W3: Case Study

RNA-seq reproducibility workflow:
1. Load and tag raw counts
2. Normalize (v1) with log1p
3. Multi-table join with metadata
4. Re-normalize (v2) with scaling
5. Compare versions with `diff()`
6. Explore lineage with `deps/tree`

## Output

Results are written to `results/`:

- `benchmark_*.jsonl` - Raw measurement records (JSONL)
- `case_study_*.jsonl` - Case study timings
- `summary_*.csv` - Aggregated statistics
- `figures/` - Generated plots (PNG/PDF/SVG)
- `*_report.md` - Markdown reports

### JSONL Record Schema

```json
{
  "run_id": "uuid",
  "timestamp": "ISO8601",
  "workload": "W1-2",
  "variant": "omicslake|baseline_duckdb|file_based",
  "size": "small|medium|large",
  "cache": "cold|warm|na",
  "rep": 1,
  "metrics": {
    "time_sec": 0.123,
    "rss_mb": 45.6,
    "bytes_delta": 123456,
    "n_rows": 1000
  },
  "env": { ... },
  "evidence": {
    "sql": "SELECT ... WHERE ...",
    "pushdown_valid": true
  }
}
```

## Pass/Fail Criteria

### Functional Gates

1. **W1-1/W1-2**: SQL pushdown evidence (WHERE/SELECT in SQL)
2. **W3**: `deps()` returns `parent_ref` and `parent_version_id`
3. **W3**: Multi-parent joins preserve all dependencies
4. **W3**: `diff()` returns version-specific differences

### Performance Gates (Paper-specific)

- Overhead vs baseline: within acceptable range (defined per paper)
- Storage growth: explainable, linear/stepwise

## R API

```r
# Load configuration
config <- ol_eval_load_config("inst/eval/configs/eval_small.yml")

# Run all benchmarks
ol_eval_run_benchmarks(config, output_file = "results.jsonl")

# Run case study
results <- ol_eval_run_case_study(config)

# Generate plots
ol_eval_plot_all("results.jsonl", output_dir = "figures/")

# Aggregate results
ol_eval_aggregate_results("results.jsonl", "summary.csv")
```

## Development

### Running Tests

```bash
# Run evaluation contract tests
Rscript -e "devtools::test(filter = 'eval-contracts')"
```

### Adding New Workloads

1. Add workload function in `R/eval_bench.R` (e.g., `.ol_eval_W1_new()`)
2. Call from appropriate runner (`.ol_eval_run_W1()`)
3. Add config option in YAML files
4. Add contract test if needed

## Known Limitations (v0.1)

- Cold cache measurement is approximate (environment-dependent)
- Operation-level provenance is not evaluated (dataset-level only)
- Large configurations may be skipped on limited hardware

## Files

```
inst/eval/
├── configs/
│   ├── eval_default.yml    # Full config documentation
│   ├── eval_small.yml      # Quick testing
│   ├── eval_medium.yml     # Paper-grade
│   └── eval_large.yml      # Comprehensive
├── scripts/
│   ├── run_eval.R          # Full evaluation
│   ├── run_benchmarks.R    # W0-W2 only
│   ├── run_case_study.R    # W3 only
│   └── plot_results.R      # Generate figures
├── case_study/
│   ├── rnaseq_case_study.Rmd
│   └── assets/
├── results/                 # Output (gitignored)
│   └── .gitignore
└── README.md               # This file
```

## Related R Source Files

- `R/eval_metrics.R` - Config, metrics, JSONL I/O
- `R/eval_generate.R` - Synthetic data generators
- `R/eval_bench.R` - W0-W2 workload implementations
- `R/eval_case_study.R` - W3 case study
- `R/eval_plot.R` - Figure generation
