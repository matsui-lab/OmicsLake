# CLAUDE.md - OmicsLake Development Guide

## Project Overview

**OmicsLake** is an R package for versioned, reproducible omics data management with automatic data lineage tracking. Built on DuckDB, Apache Arrow, and Parquet, it provides a lightweight addon for bioinformatics workflows.

### Core Value Proposition
- Automatic lineage tracking through dplyr pipes
- Version control for data (snapshots, tags, time travel)
- Zero-friction integration with existing R/Bioconductor workflows
- R-native query syntax (formulas, dplyr) - no SQL required

## Repository Structure

```
OmicsLake/
├── R/                          # Package source code
│   ├── Lake.R                  # Main R6 class (v2.0 API)
│   ├── QueryBuilder.R          # Fluent query builder
│   ├── shortcuts.R             # Global convenience functions (put, get, snap, tree)
│   ├── operators.R             # Custom operators (%like%, %between%, etc.)
│   ├── dplyr_compat.R          # dplyr integration (save_as, lake_tbl class)
│   ├── observe.R               # Lightweight mode for non-invasive tracking
│   ├── compat.R                # Backward compatibility (ol_* to v2.0 mapping)
│   ├── io.R                    # Core I/O functions (ol_write, ol_read, etc.)
│   ├── init.R                  # Project initialization (ol_init, ol_label)
│   ├── backend.R               # DuckDB backend infrastructure
│   ├── versions.R              # Version management (tags, snapshots)
│   ├── bioc.R                  # Bioconductor support (SummarizedExperiment)
│   ├── query.R                 # SQL query execution
│   ├── views.R                 # View management
│   ├── parquet.R               # Parquet import/export
│   ├── visualization.R         # Lineage visualization
│   ├── utils.R                 # Internal utilities
│   ├── zzz.R                   # Package load hooks
│   └── adapters/               # Type-specific adapters
│       ├── base.R              # LakeAdapter base class
│       └── se_adapter.R        # SummarizedExperiment adapter
├── man/                        # Auto-generated documentation (roxygen2)
├── tests/
│   ├── testthat.R              # Test runner
│   └── testthat/               # Test files
│       ├── test-basic.R        # Package loading test
│       ├── test-lake.R         # Lake R6 class tests
│       ├── test-io.R           # I/O operations tests
│       ├── test-versions.R     # Versioning tests
│       ├── test-query.R        # Query functionality tests
│       ├── test-querybuilder.R # QueryBuilder tests
│       ├── test-operators.R    # Custom operators tests
│       ├── test-lightweight.R  # Observe mode tests
│       ├── test-bioc.R         # Bioconductor integration tests
│       └── ...                 # Additional test files
├── vignettes/                  # User documentation (Rmd)
├── inst/paper/                 # Benchmark and reproducibility scripts
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Export declarations
└── CLAUDE_IMPLEMENTATION_GUIDE.md  # Detailed v2.0 implementation spec
```

## Key Concepts

### 1. Dual API System
OmicsLake has two API layers:

**Legacy API (v1.0)** - Prefix: `ol_*`
```r
ol_init("project")
ol_write("table", df)
ol_read("table")
ol_label("v1.0")
```

**Modern API (v2.0)** - R6 class: `Lake`
```r
lake <- Lake$new("project")
lake$put("table", df)
lake$get("table")
lake$snap("v1.0")
```

Both APIs are fully functional. The legacy API internally uses the same backend.

### 2. Storage Types
- **Tables**: Data frames stored in DuckDB (queryable with SQL/dplyr)
- **Objects**: Arbitrary R objects serialized with qs or RDS
- **SummarizedExperiment**: Bioconductor objects (via SEAdapter)

### 3. Version References
- `@latest` - Most recent version (default)
- `@first` - First version
- `@tag(name)` - Tagged version
- Timestamp - Specific point in time

### 4. Dependency Tracking
Dependencies are tracked:
- **Explicitly**: via `depends_on` parameter
- **Automatically**: through dplyr pipes using `lake$ref()` and `save_as()`

### 5. Internal Tables
The backend maintains metadata in special tables:
- `__ol_refs` - Tags and snapshot references
- `__ol_objects` - Serialized R objects
- `__ol_commits` - Commit history with notes/params
- `__ol_dependencies` - Lineage graph edges

## Development Workflow

### Running Tests
```bash
# Run all tests
cd /home/user/OmicsLake
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test-lake.R')"

# Run with filter
Rscript -e "devtools::test(filter = 'lake')"
```

### Package Checks
```bash
R CMD check .
# or
Rscript -e "devtools::check()"
```

### Documentation
```bash
# Regenerate documentation from roxygen2 comments
Rscript -e "devtools::document()"
```

### Building the Package
```bash
R CMD build .
R CMD INSTALL OmicsLake_*.tar.gz
```

## Code Conventions

### Naming Conventions
- Legacy functions: `ol_<verb>()` (e.g., `ol_write()`, `ol_read()`)
- Internal functions: `.ol_<name>()` (e.g., `.ol_validate_name()`)
- R6 class methods: camelCase for internal, lowercase for public
- Test files: `test-<feature>.R`

### R6 Class Pattern
```r
ClassName <- R6::R6Class("ClassName",
  public = list(
    initialize = function(...) { },
    public_method = function(...) { invisible(self) }  # Return self for chaining
  ),
  private = list(
    .private_field = NULL,
    .private_method = function(...) { }
  )
)
```

### Documentation Style
- Use roxygen2 for all exported functions
- Include `@export` for public functions
- Include `@examples` with `\dontrun{}` for examples requiring setup
- Japanese comments in implementation guide, English in code

### Error Handling
```r
# Use call. = FALSE for cleaner error messages
stop("Error message", call. = FALSE)
warning("Warning message", call. = FALSE)

# Validate inputs early
.ol_validate_name(name, "table name")
.ol_assert_project(project, "Call ol_init() first")
```

### Test Structure
```r
test_that("descriptive test name", {
  lake <- Lake$new("test_unique_name")

  # Test code
  expect_equal(result, expected)

  # Always clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_unique_name"), recursive = TRUE)
})
```

## Key Files Reference

### R/Lake.R (lines 1-804)
Main R6 class implementing the v2.0 API. Contains:
- `Lake` class: Core data operations (put, get, ref, snap, tag, tree)
- `DependencyTracker` class: Automatic lineage tracking

### R/io.R (lines 1-385)
Core I/O operations:
- `ol_write()` - Write tables to DuckDB
- `ol_read()` - Read tables with version support
- `ol_save()` - Save arbitrary R objects
- `ol_list_tables()`, `ol_list_objects()` - Listing functions
- `ol_get_dependencies()`, `ol_show_lineage()` - Lineage queries

### R/backend.R
Backend infrastructure:
- Connection management
- Schema creation
- Internal table management

### R/shortcuts.R
Global convenience functions:
- `use_lake()` - Set default lake
- `put()`, `fetch()`, `ref()` - Default lake operations
- `snap()`, `tag()`, `tree()` - Version control shortcuts

### R/operators.R
Custom operators:
- `%like%`, `%ilike%` - Pattern matching
- `%between%`, `%!between%` - Range filtering
- `%regex%`, `%iregex%` - Regex matching
- `is_null()`, `is_not_null()` - NULL checks

### R/dplyr_compat.R
dplyr integration:
- `save_as()` - Pipe terminator to save results
- `lake_tbl` S3 class with method overrides
- Lineage tracking through dplyr verbs

### R/observe.R
Lightweight mode:
- `observe()` - Track operations without storing data
- `wrap_fn()` - Wrap functions for lineage tracking
- `mark()`, `link()` - Manual lineage annotations
- `Pipeline` class - Define data pipelines

## NAMESPACE Exports

### v2.0 Core Classes
- `Lake` - Main R6 class
- `QueryBuilder` - Fluent query builder
- `DependencyTracker` - Lineage tracker

### v2.0 Shortcuts
- `use_lake`, `lake`, `put`, `fetch`, `ref`
- `snap`, `tag`, `tree`, `history`
- `tables`, `objects`, `drop`, `sql`, `restore`
- `deps`, `import_data`, `export_data`, `query`, `from`

### v2.0 Operators
- `%like%`, `%ilike%`, `%between%`, `%!between%`
- `%!in%`, `%regex%`, `%iregex%`
- `is_null`, `is_not_null`, `starts_with_str`, `ends_with_str`, `contains_str`
- `coalesce`, `if_else_na`

### v2.0 dplyr Compatibility
- `save_as`, `into`, `%>>%`

### v2.0 Lightweight Mode
- `observe`, `observe_to_lake`, `with_tracking`, `observe_session`
- `wrap_fn`, `wrap_call`, `mark`, `link`, `unlink_dep`
- `create_pipeline`, `Pipeline`, `trace_calls`
- `ObserveSession`

### v2.0 Adapters
- `LakeAdapter`, `SEAdapter`
- `register_adapter`, `get_adapters`, `find_adapter`

### Legacy API (ol_* functions)
All `ol_*` functions are maintained for backward compatibility.

## Dependencies

### Imports (Required)
- `R6` - Object-oriented programming
- `DBI`, `duckdb` - Database backend
- `arrow` - Parquet/Arrow support
- `dplyr`, `dbplyr` - Data manipulation
- `rlang` - Tidy evaluation
- `jsonlite` - JSON serialization
- `digest` - Hashing
- `Matrix` - Sparse matrix support
- `SummarizedExperiment`, `S4Vectors` - Bioconductor base
- `tools` - File utilities

### Suggests (Optional)
- `qs` - Fast serialization
- `MultiAssayExperiment`, `HDF5Array`, `DelayedArray` - Advanced Bioconductor
- `igraph` - Lineage visualization
- `testthat`, `knitr`, `rmarkdown` - Development

## Common Tasks

### Adding a New Export
1. Add `@export` roxygen tag to function
2. Run `devtools::document()` to update NAMESPACE
3. Add tests in appropriate test file

### Modifying Lake Class
1. Edit `R/Lake.R`
2. Public methods should return `invisible(self)` for chaining
3. Use `private$.method_name()` for internal logic
4. Update tests in `tests/testthat/test-lake.R`

### Adding a New Operator
1. Add function to `R/operators.R`
2. Add `@export` tag
3. Add to NAMESPACE exports
4. Add tests to `tests/testthat/test-operators.R`

### Adding Bioconductor Support
1. Check adapter pattern in `R/adapters/`
2. Create new adapter inheriting from `LakeAdapter`
3. Register in `R/zzz.R` or adapter initialization
4. Add tests to `tests/testthat/test-bioc.R`

## Storage Locations

Default data storage: `~/.omicslake/<project>/`

Each project contains:
- `db.duckdb` - DuckDB database file
- Object storage (if using external mode)

## Tips for AI Assistants

1. **Prefer the v2.0 API** (`Lake` class) for new code
2. **Always clean up test data** in test files
3. **Use unique project names** in tests to avoid conflicts
4. **Check both APIs** when modifying core functionality
5. **Run tests** after any changes: `devtools::test()`
6. **Regenerate docs** after changing roxygen comments
7. **The CLAUDE_IMPLEMENTATION_GUIDE.md** contains detailed specs (in Japanese)
8. **Backend state** is stored in `.ol_backend_registry` environment
9. **lake_tbl class** preserves lineage through dplyr operations
10. **Dependency tracking** uses a stack-based approach in DependencyTracker
