# OmicsLake User Vignette

This vignette walks you through setting up the development version of OmicsLake on your local machine, generating demo data, and using the main APIs provided by the package.

## 1. Environment setup

### 1.1 Required software

- R (version 4.3 or later recommended)
- C++ build tools (may be required to build the duckdb package)
- Git (if obtaining the development version)

### 1.2 Installing dependencies

OmicsLake uses DuckDB and Apache Arrow as backends. Please install the following packages beforehand:

```r
install.packages(c(
  "arrow",
  "duckdb",
  "DBI",
  "dplyr" # useful if you want to query the database using dplyr
))
````

The development version of the package itself can be installed using `remotes`:

```r
install.packages("remotes")
remotes::install_local("path/to/OmicsLake")
```

If you are working from the source, you can also load it with `devtools::load_all()`.

## 2. Project initialization

OmicsLake manages a DuckDB catalog for each project. Running `ol_init()` creates a project-specific working directory and DuckDB database, linking subsequent operations to that project.

```r
library(OmicsLake)

# Create and connect to a project named "atlas"
ol_init("atlas")
```

Internally, it connects to DuckDB and sets up the catalog and schema.

> **Tip:** You can change the project root path using `options(ol.root = "/path/to/root")`.

## 3. Generating demo data

The following code generates a mock RNA-seq count table for demonstration purposes.

```r
set.seed(42)
samples <- paste0("sample", sprintf("%02d", 1:6))
genes <- paste0("gene", sprintf("%03d", 1:500))
counts <- matrix(rpois(length(samples) * length(genes), lambda = 50),
                 nrow = length(genes), dimnames = list(genes, samples))
counts_df <- as.data.frame(counts, stringsAsFactors = FALSE)
counts_df$gene_id <- rownames(counts_df)
counts_df <- counts_df[, c("gene_id", samples)]
```

## 4. Writing data to the database

To store the generated data frame as a database table, use `ol_write()`.
This function registers it as a temporary Arrow table and writes it into DuckDB.

```r
# Use mode = "create" for the first run; mode = "overwrite" to replace an existing table
ol_write("counts", counts_df, mode = "create")
```

> **Note:** If you use `mode = "append"`, new data will be appended to the existing table.

## 5. Snapshot management and labeling

OmicsLake automatically creates a snapshot upon every write.
`ol_commit()` returns its ID (usually a timestamp string).

```r
snapshot_id <- ol_commit("counts import")
```

You can assign a human-readable label using `ol_label()`.
Internally, metadata including tag information is stored.

```r
ol_label("baseline")
```

You can also attach tags to specific table snapshots using `ol_tag()`:

```r
ol_tag("counts", tag = "qc-passed", ref = snapshot_id)
```

## 6. Reading tables

To retrieve the latest snapshot, use `ol_read()`.
It interprets reference syntax (`@latest`, `@tag(name)`, `@version(timestamp)`)
and generates the corresponding SQL query to return the data.

```r
counts_latest <- ol_read("counts")                 # latest version
counts_tagged <- ol_read("counts", ref = "@tag(qc-passed)")  # by tag
```

If you specify `collect = FALSE`, it returns a `dplyr::tbl` reference for lazy evaluation.

You can check the snapshot history with `ol_log()`:

```r
ol_log("counts")
```

## 7. Saving and loading R objects

If you want to manage R objects instead of tables, use `ol_save()` and `ol_read_object()`.
By default, objects are serialized and stored inside the database, but in external storage mode they are saved as `.rds` files and referenced by their file paths.

```r
metadata <- list(species = "human", build = "GRCh38")
ol_save("counts_metadata", metadata)
restored <- ol_read_object("counts_metadata")
```

To retrieve the first saved version of an object, specify `when = "first"`:

```r
initial_copy <- ol_read_object("counts_metadata", when = "first")
```

## 8. Cleaning up at the end of a session

To clean up an unused project, simply close the DuckDB connection.

```r
ol_disconnect <- function(project = getOption("ol.project")) {
  OmicsLake:::.ol_disconnect_backend(project)
}

ol_disconnect()
```

`.ol_disconnect_backend()` is an internal function that safely closes the DuckDB connection and removes its registry entry.

---

By following these steps, you now understand the full workflow of initializing a project, writing and reading data, and managing metadata using OmicsLake.
Feel free to adapt this process to your own data and workflows.

