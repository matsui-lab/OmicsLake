# OmicsLake

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**OmicsLake** is an R package for versioned, reproducible omics data management with automatic data lineage tracking. Built on DuckDB, Apache Arrow, and Parquet, it seamlessly integrates into existing bioinformatics workflows as a lightweight addon.

## Features

- **Automatic Lineage Tracking** - Dependencies tracked through dplyr pipes without manual annotation
- **Simple API** - Just `put()`, `get()`, `snap()`, `tree()` - no verbose function names
- **R-Native Queries** - No SQL required; use formula syntax or dplyr
- **Bioconductor Ready** - Full SummarizedExperiment and Seurat support
- **Zero Friction** - Drop into existing workflows with minimal code changes
- **Time Travel** - Tag versions, create snapshots, restore any state

## Installation

```r
# From GitHub
remotes::install_github("matsui-lab/OmicsLake")
```

## Quick Start

```r
library(OmicsLake)

# Initialize a lake
lake <- Lake$new("my_analysis")

# Store data
lake$put("counts", counts_df)
lake$put("metadata", sample_info)

# Process with dplyr (dependencies auto-tracked!)
lake$ref("counts") |>
  dplyr::left_join(lake$ref("metadata"), by = "sample_id") |>
  dplyr::filter(quality > 0.8) |>
  dplyr::group_by(condition) |>
  dplyr::summarize(mean_expr = mean(expression)) |>
  save_as("summary", lake)

# View lineage
lake$tree("summary")
#> counts -> summary
#> metadata -> summary

# Create snapshot
lake$snap("v1.0")

# Restore later
lake$restore("v1.0")
```

## API Overview

### Core Operations

```r
lake <- Lake$new("project")   # Initialize
lake$put("data", df)          # Store (tables or objects)
lake$get("data")              # Retrieve
lake$ref("data")              # Get lazy reference for dplyr
lake$drop("data")             # Remove
```

### Versioning

```r
lake$snap("v1.0")             # Create snapshot
lake$tag("data", "raw")       # Tag specific data
lake$restore("v1.0")          # Restore snapshot
lake$get("data", ref="@tag(raw)")  # Get tagged version
```

### Lineage

```r
lake$tree("data")             # Show lineage tree
lake$plot("data")             # Visualize lineage graph
lake$deps("data")             # Get direct dependencies
lake$impact("data")           # Analyze downstream impact
```

### Querying

```r
# Formula syntax
lake$get("counts", where = ~ gene %like% "MT-%" & expression > 100)

# Query Builder
lake$from("counts")$
  join("annotations", on = "gene_id")$
  where(biotype == "protein_coding")$
  top(100, by = expression)$
  run()

# Bracket notation
lake["counts"]                    # Read all
lake["counts", x > 5]             # With filter
lake["counts", x > 5, .(a, b)]    # Filter + select
```

### Custom Operators

```r
# Pattern matching (SQL LIKE)
genes[genes %like% "MT-%"]

# Range filtering
values[values %between% c(10, 100)]

# Case-insensitive matching
names[names %ilike% "john%"]

# Regex matching
genes[genes %regex% "^ENSG"]
```

### Global Shortcuts

```r
use_lake("project")           # Set default lake
put("data", df)               # Uses default lake
fetch("data")                 # Uses default lake
snap("v1.0")
tree("data")
```

## dplyr Integration

Dependencies are automatically tracked through dplyr operations:

```r
lake$ref("raw_counts") |>
  dplyr::filter(quality > 0.8) |>
  dplyr::left_join(lake$ref("metadata"), by = "sample_id") |>
  dplyr::group_by(condition) |>
  dplyr::summarize(mean_expr = mean(expression)) |>
  save_as("summary_stats", lake)

# Lineage automatically recorded:
# raw_counts + metadata -> summary_stats
```

## Bioconductor Support

```r
# Store SummarizedExperiment (all components preserved)
lake$put("rna_data", se)

# Retrieve with full fidelity
se_restored <- lake$get("rna_data")

# assays, colData, rowData, metadata all preserved
```

## Version Control Workflow

```r
# Stage 1: Raw data
lake$put("raw", raw_df)
lake$snap("stage1_raw")

# Stage 2: QC filtered
lake$put("filtered", filtered_df, depends_on = "raw")
lake$snap("stage2_qc")

# Stage 3: Normalized
lake$put("normalized", norm_df, depends_on = "filtered")
lake$snap("stage3_normalized")

# Rollback to any stage
lake$restore("stage2_qc")

# Compare versions
lake$diff("normalized")
```

## Migration from v1 API

The legacy `ol_*` functions continue to work but the new API is recommended:

| Legacy | New |
|--------|-----|
| `ol_init("proj")` | `Lake$new("proj")` |
| `ol_write("t", df)` | `lake$put("t", df)` |
| `ol_read("t")` | `lake$get("t")` |
| `ol_label("v1")` | `lake$snap("v1")` |
| `ol_show_lineage("t")` | `lake$tree("t")` |
| `ol_query("SQL")` | `lake$sql("SQL")` |

See `show_migration_guide()` for the complete mapping.

## Architecture

OmicsLake leverages:

- **DuckDB** - Fast in-process analytical database
- **Apache Arrow** - Zero-copy data sharing
- **Parquet** - Efficient columnar storage
- **R6** - Modern object-oriented programming

Data is stored in project-specific DuckDB databases with automatic snapshot and dependency tracking.

## Contributing

Contributions welcome! Please open issues or PRs at:
https://github.com/matsui-lab/OmicsLake

## License

MIT License. See LICENSE file.

## Citation

```
Matsui, Y. (2024). OmicsLake: Versioned, Reproducible Omics Data Management for R.
GitHub: https://github.com/matsui-lab/OmicsLake
```
