## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    eval = isTRUE(getOption("omicslake.vignette.eval", FALSE))
)

## ----install------------------------------------------------------------------
# # Install from GitHub
# # Installation command is documented in README.md

## ----init---------------------------------------------------------------------
# library(OmicsLake)
# 
# # Create/open a project
# lake <- Lake$new("my_analysis")
# 
# # Or use global shortcuts
# use_lake("my_analysis")

## ----basic_io-----------------------------------------------------------------
# # Store a data frame
# counts <- data.frame(
#     gene_id = paste0("GENE", 1:100),
#     sample_A = rpois(100, 50),
#     sample_B = rpois(100, 60)
# )
# lake$put("counts", counts)
# 
# # R objects can also be stored
# params <- list(
#     method = "TMM",
#     log_transform = TRUE,
#     threshold = 0.05
# )
# lake$put("analysis_params", params)
# 
# # Read data
# data <- lake$get("counts")
# my_params <- lake$get("analysis_params")

## ----formula_filter-----------------------------------------------------------
# # No SQL needed! Filter with formula syntax
# high_expr <- lake$get("counts", where = ~ sample_A > 50)
# 
# # Custom operators are also available
# mito_genes <- lake$get("counts", where = ~ gene_id %like% "MT-%")
# 
# # Compound conditions
# filtered <- lake$get("counts",
#     where = ~ sample_A > 30 & sample_B %between% c(40, 80)
# )
# 
# # Select columns simultaneously
# subset <- lake$get("counts",
#     where = ~ sample_A > 50,
#     select = c("gene_id", "sample_A")
# )

## ----dplyr_integration--------------------------------------------------------
# # Get a lazy reference with lake$ref()
# # Dependencies in dplyr pipes are automatically tracked!
# library(dplyr)
# 
# lake$ref("counts") |>
#     filter(sample_A > 30) |>
#     mutate(
#         mean_expr = (sample_A + sample_B) / 2,
#         log2_ratio = log2(sample_B / sample_A)
#     ) |>
#     arrange(desc(mean_expr)) |>
#     save_as("processed_counts", lake)
# 
# # Check lineage
# lake$tree("processed_counts")
# # counts -> processed_counts

## ----join_example-------------------------------------------------------------
# # Add metadata
# metadata <- data.frame(
#     gene_id = paste0("GENE", 1:100),
#     gene_name = paste0("Gene_", LETTERS[1:4])[rep(1:4, 25)],
#     biotype = sample(c("protein_coding", "lncRNA"), 100, replace = TRUE)
# )
# lake$put("gene_metadata", metadata)
# 
# # JOIN with dplyr
# lake$ref("counts") |>
#     left_join(lake$ref("gene_metadata"), by = "gene_id") |>
#     filter(biotype == "protein_coding") |>
#     group_by(gene_name) |>
#     summarize(total_A = sum(sample_A), total_B = sum(sample_B)) |>
#     save_as("gene_summary", lake)
# 
# # Dependencies are automatically tracked
# lake$tree("gene_summary")
# # counts -----> gene_summary
# # gene_metadata ↗

## ----querybuilder-------------------------------------------------------------
# # Build queries with method chaining
# result <- lake$from("counts")$
#     join("gene_metadata", on = "gene_id")$
#     where(biotype == "protein_coding")$
#     where(sample_A > 40)$
#     select(gene_id, gene_name, sample_A, sample_B)$
#     order_by(desc(sample_A))$
#     top(20, by = sample_A)$
#     run()
# 
# # Save results to Lake
# lake$from("counts")$
#     where(sample_A > 50)$
#     as("high_expression_genes")

## ----versioning---------------------------------------------------------------
# # Snapshot current state
# lake$snap("v1.0_raw_data")
# 
# # Update data
# normalized <- lake$get("counts")
# normalized$sample_A <- log2(normalized$sample_A + 1)
# normalized$sample_B <- log2(normalized$sample_B + 1)
# lake$put("counts", normalized)
# 
# lake$snap("v1.1_normalized")
# 
# # Tag individual data
# lake$tag("counts", "before_normalization")
# 
# # Get past version
# original <- lake$get("counts", ref = "@tag(before_normalization)")
# 
# # Restore to snapshot
# lake$restore("v1.0_raw_data")

## ----history------------------------------------------------------------------
# # Project history
# lake$log()
# 
# # History of specific data
# lake$log("counts")
# 
# # List snapshots
# lake$snaps()

## ----lineage------------------------------------------------------------------
# # Upstream dependencies (what was this data made from?)
# lake$deps("gene_summary", direction = "up")
# 
# # Downstream dependencies (what uses this data?)
# lake$deps("counts", direction = "down")
# 
# # Complete lineage tree
# lake$tree("gene_summary", direction = "up", depth = 10)
# 
# # Impact analysis (what would be affected if this data changes?)
# lake$impact("counts")

## ----plot_lineage-------------------------------------------------------------
# # Visualize as graph (requires igraph package)
# lake$plot("gene_summary", direction = "both")

## ----observe_mode-------------------------------------------------------------
# # Run existing scripts in observation mode
# result <- observe({
#     data <- read.csv("input_data.csv")
#     processed <- data[data$value > 0, ]
#     write.csv(processed, "output_data.csv")
# })
# 
# # Files read and written are recorded
# print(result$reads)
# print(result$writes)
# print(result$lineage)

## ----wrap_mode----------------------------------------------------------------
# # Wrap an existing function
# normalize_data <- function(x) {
#     x$normalized <- scale(x$value)
#     x
# }
# 
# tracked_normalize <- wrap_fn(normalize_data, lake, "normalized_result")
# 
# # Use the wrapped function - lineage is automatically recorded
# result <- tracked_normalize(my_data)

## ----pipeline-----------------------------------------------------------------
# pipeline <- create_pipeline(lake, "preprocessing")
# 
# pipeline$
#     step("load", function() read.csv("data.csv"))$
#     step("clean", function(data) na.omit(data))$
#     step("normalize", function(data) {
#     data$value <- scale(data$value)
#     data
# })$
#     step("filter", function(data) data[data$quality > 0.8, ])
# 
# result <- pipeline$run()
# 
# # Each step is recorded in the Lake
# lake$tree("preprocessing.filter")

## ----operators----------------------------------------------------------------
# # %like% - SQL LIKE pattern matching
# genes[genes %like% "MT-%"] # Genes starting with MT-
# genes[genes %like% "%kinase%"] # Contains "kinase"
# 
# # %ilike% - Case-insensitive
# names[names %ilike% "john%"]
# 
# # %between% - Range filter
# values[values %between% c(10, 100)]
# 
# # %regex% - Regular expression matching
# ids[ids %regex% "^ENSG\\d{11}$"]
# 
# # %!in% - NOT IN
# letters[letters %!in% c("a", "e", "i", "o", "u")]
# 
# # is_null / is_not_null
# data[is_not_null(data$value), ]

## ----bracket_notation---------------------------------------------------------
# # Simple read/write
# lake["counts"] # Read all data
# lake["new_data"] <- df # Write
# 
# # Read with filter
# lake["counts", sample_A > 50]
# 
# # Filter + column selection
# lake["counts", sample_A > 50, .(gene_id, sample_A)]

## ----import_export------------------------------------------------------------
# # Parquet export
# lake$export("counts", "counts.parquet")
# 
# # CSV export
# lake$export("counts", "counts.csv")
# 
# # Import
# lake$import("external_data.parquet", "imported_data")
# lake$import("annotations.csv", "gene_annotations")

## ----bioconductor-------------------------------------------------------------
# library(SummarizedExperiment)
# 
# # Store SummarizedExperiment directly
# # assays, colData, rowData, metadata are all preserved
# lake$put("rna_experiment", se_object)
# 
# # Restore completely
# se_restored <- lake$get("rna_experiment")

## ----shortcuts----------------------------------------------------------------
# # Set default Lake
# use_lake("my_project")
# 
# # Operate without lake$ prefix
# put("data", df)
# data <- fetch("data") # fetch() instead of get()
# snap("checkpoint1")
# tree("data")
# tables()
# history()

## ----migration----------------------------------------------------------------
# # Show migration guide
# show_migration_guide()

## ----help---------------------------------------------------------------------
# ?Lake
# ?QueryBuilder
# ?observe
# ?wrap_fn

## ----session_info-------------------------------------------------------------
# sessionInfo()

