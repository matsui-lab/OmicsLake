## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    eval = isTRUE(getOption("omicslake.vignette.eval", FALSE)),
    fig.width = 7,
    fig.height = 5
)

## ----init---------------------------------------------------------------------
# library(OmicsLake)
# 
# # Initialize a project
# ol_init("de_analysis")

## ----tables-------------------------------------------------------------------
# # Create sample data
# set.seed(123)
# raw_counts <- data.frame(
#     gene_id = paste0("GENE", 1:100),
#     sample1 = rpois(100, 100),
#     sample2 = rpois(100, 100),
#     sample3 = rpois(100, 120),
#     sample4 = rpois(100, 120)
# )
# 
# # Save as a table
# ol_write("raw_counts", raw_counts, mode = "create")
# 
# # Read the table
# counts <- ol_read("raw_counts")
# head(counts)
# 
# # List tables
# ol_list_tables()

## ----query_basic--------------------------------------------------------------
# # Basic query
# result <- ol_query("SELECT * FROM raw_counts WHERE sample1 > 100")
# head(result)
# 
# # Aggregation query
# summary <- ol_query("\
#     SELECT
#     COUNT(*) as total_genes,
#     AVG(sample1) as avg_sample1,
#     MAX(sample1) as max_sample1
#     FROM raw_counts
# ")
# print(summary)

## ----query_join---------------------------------------------------------------
# # Join tables (example: join with gene annotation data)
# ol_write("gene_annotations", data.frame(
#     gene_id = paste0("gene", 1:5),
#     name = paste0("GeneA", 1:5),
#     chromosome = c("chr1", "chr2", "chr1", "chr3", "chr2")
# ))
# 
# joined <- ol_query("\
#     SELECT r.gene_id, r.sample1, a.name, a.chromosome
#     FROM raw_counts r
#     JOIN gene_annotations a ON r.gene_id = a.gene_id
#     WHERE r.sample1 > 50
# ")
# head(joined)

## ----query_lazy---------------------------------------------------------------
# # collect=FALSE for lazy evaluation
# if (requireNamespace("dplyr", quietly = TRUE)) {
#     lazy_tbl <- ol_query("SELECT * FROM raw_counts", collect = FALSE)
# 
#     # Process with dplyr
#     result <- lazy_tbl %>%
#         dplyr::filter(sample1 > 100) %>%
#         dplyr::select(gene_id, sample1, sample2) %>%
#         dplyr::arrange(dplyr::desc(sample1)) %>%
#         dplyr::collect()
# 
#     head(result)
# }

## ----aggregate_stats----------------------------------------------------------
# # Statistics across all samples
# overall_stats <- ol_aggregate("raw_counts",
#     mean_sample1 = list(func = "avg", col = "sample1"),
#     median_sample1 = list(func = "median", col = "sample1"),
#     sd_sample1 = list(func = "stddev", col = "sample1")
# )
# print(overall_stats)
# 
# # Statistics by category (e.g., high vs low expression in sample1)
# # First add a category for high/low expression
# ol_query("\
#     CREATE OR REPLACE TABLE raw_counts_categorized AS
#     SELECT *,
#     CASE WHEN sample1 > 100 THEN 'high' ELSE 'low' END as expr_category
#     FROM raw_counts
# ")
# 
# grouped_stats <- ol_aggregate("raw_counts_categorized",
#     group_by = "expr_category",
#     count = list(func = "count", col = "*"),
#     mean_s1 = list(func = "avg", col = "sample1"),
#     mean_s2 = list(func = "avg", col = "sample2")
# )
# print(grouped_stats)

## ----ranking------------------------------------------------------------------
# # Rank genes by expression
# ranked_genes <- ol_add_rank("raw_counts",
#     rank_by = "sample1",
#     method = "row_number",
#     descending = TRUE,
#     as_column = "expression_rank"
# )
# head(ranked_genes)
# 
# # Get the top 10 genes
# top_genes <- ol_top_n("raw_counts",
#     n = 10,
#     order_by = "sample1",
#     descending = TRUE
# )
# print(top_genes)
# 
# # Top 5 genes per sample
# # Convert to long format first
# ol_query("\
#     CREATE OR REPLACE TABLE raw_counts_long AS
#     SELECT gene_id, 'sample1' as sample, sample1 as expression FROM raw_counts
#     UNION ALL
#     SELECT gene_id, 'sample2' as sample, sample2 as expression FROM raw_counts
#     UNION ALL
#     SELECT gene_id, 'sample3' as sample, sample3 as expression FROM raw_counts
#     UNION ALL
#     SELECT gene_id, 'sample4' as sample, sample4 as expression FROM raw_counts
# ")
# 
# top_per_sample <- ol_top_n("raw_counts_long",
#     n = 5,
#     order_by = "expression",
#     partition_by = "sample",
#     descending = TRUE
# )
# print(top_per_sample)

## ----moving_cumulative--------------------------------------------------------
# # Moving average (smoothing)
# # Sort by gene ID and compute a 3-gene moving average
# smoothed <- ol_moving_avg("raw_counts",
#     column = "sample1",
#     window_size = 3,
#     order_by = "gene_id",
#     as_column = "sample1_smoothed"
# )
# head(smoothed, 10)
# 
# # Larger window size
# smoothed_5 <- ol_moving_avg("raw_counts",
#     column = "sample1",
#     window_size = 5,
#     order_by = "gene_id",
#     as_column = "sample1_ma5"
# )
# head(smoothed_5, 10)
# 
# # Cumulative sum (sorted by gene ID)
# cumulative <- ol_cumulative_sum("raw_counts",
#     column = "sample1",
#     order_by = "gene_id",
#     as_column = "sample1_cumsum"
# )
# head(cumulative, 10)
# tail(cumulative)
# 
# # Cumulative sum per sample (using the long-format data)
# cumulative_per_sample <- ol_cumulative_sum("raw_counts_long",
#     column = "expression",
#     partition_by = "sample",
#     order_by = "gene_id",
#     as_column = "cumulative_expr"
# )
# head(cumulative_per_sample, 20)

## ----lazy_aggregation---------------------------------------------------------
# # Aggregation functions also support lazy evaluation
# if (requireNamespace("dplyr", quietly = TRUE)) {
#     # Compute statistics lazily
#     lazy_stats <- ol_aggregate("raw_counts",
#         mean_s1 = list(func = "avg", col = "sample1"),
#         collect = FALSE
#     )
# 
#     # Further process with dplyr
#     result <- lazy_stats %>%
#         dplyr::collect()
#     print(result)
# 
#     # Combine with ranking
#     lazy_ranked <- ol_add_rank("raw_counts",
#         rank_by = "sample1",
#         as_column = "rank",
#         collect = FALSE
#     )
# 
#     top_ranked <- lazy_ranked %>%
#         dplyr::filter(rank <= 10) %>%
#         dplyr::select(gene_id, sample1, rank) %>%
#         dplyr::arrange(rank) %>%
#         dplyr::collect()
#     print(top_ranked)
# }

## ----objects------------------------------------------------------------------
# # Compute normalization factors (example)
# norm_factors <- list(
#     method = "TMM",
#     factors = runif(100, 0.8, 1.2)
# )
# 
# # Save as an R object
# ol_save("norm_factors", norm_factors)
# 
# # Load the object
# loaded_factors <- ol_read_object("norm_factors")
# str(loaded_factors)
# 
# # List objects
# ol_list_objects()

## ----dependencies-------------------------------------------------------------
# # Compute normalized counts (specify dependencies via depends_on)
# normalized_counts <- raw_counts
# for (i in 2:5) {
#     normalized_counts[[i]] <- normalized_counts[[i]] * norm_factors$factors
# }
# 
# # Save with dependencies
# ol_write("normalized_counts", normalized_counts,
#     depends_on = c("raw_counts", "norm_factors")
# )
# 
# # Save DE parameters
# de_params <- list(
#     method = "DESeq2",
#     alpha = 0.05,
#     lfc_threshold = 1.0
# )
# ol_save("de_params", de_params)
# 
# # Compute DE results (simplified example)
# de_results <- data.frame(
#     gene_id = paste0("GENE", 1:20),
#     log2FC = rnorm(20, 0, 2),
#     pvalue = runif(20, 0, 0.1),
#     padj = runif(20, 0, 0.1)
# )
# 
# # Save DE results with dependencies
# ol_save("de_results", de_results,
#     depends_on = c("normalized_counts", "de_params")
# )

## ----view_dependencies--------------------------------------------------------
# # Upstream dependencies (what this object depends on)
# upstream <- ol_get_dependencies("de_results", direction = "upstream")
# print(upstream)
# 
# # Downstream dependencies (what depends on this object)
# downstream <- ol_get_dependencies("raw_counts", direction = "downstream")
# print(downstream)
# 
# # Show the complete lineage tree
# lineage <- ol_show_lineage("de_results", direction = "upstream")
# print(lineage)

## ----parquet_export-----------------------------------------------------------
# # Basic export (default is Snappy compression)
# ol_export_parquet("genes", "genes.parquet")
# 
# # Export with zstd compression (high compression ratio)
# ol_export_parquet("genes", "genes_zstd.parquet",
#     compression = "zstd",
#     compression_level = 3
# )
# 
# # Specify row group size
# ol_export_parquet("genes", "genes_optimized.parquet",
#     compression = "zstd",
#     row_group_size = 50000
# )
# 
# # Uncompressed export (fastest)
# ol_export_parquet("genes", "genes_uncompressed.parquet",
#     compression = "uncompressed"
# )

## ----parquet_import-----------------------------------------------------------
# # Create a table from a Parquet file
# ol_import_parquet("genes.parquet", "imported_genes", mode = "create")
# 
# # Overwrite an existing table
# ol_import_parquet("genes_new.parquet", "genes", mode = "overwrite")
# 
# # Append data to an existing table
# ol_import_parquet("genes_batch2.parquet", "genes", mode = "append")
# 
# # Import multiple Parquet files at once
# ol_import_parquet(c("genes_part1.parquet", "genes_part2.parquet"),
#     "all_genes",
#     mode = "create"
# )
# 
# # Record dependencies when importing
# ol_import_parquet("processed_genes.parquet",
#     "final_genes",
#     depends_on = "raw_genes",
#     mode = "create"
# )

## ----parquet_performance------------------------------------------------------
# # Example optimization for a large dataset
# ol_export_parquet("large_dataset",
#     "large_dataset.parquet",
#     compression = "zstd",
#     compression_level = 1, # fastest zstd
#     row_group_size = 250000
# )

## ----tagging------------------------------------------------------------------
# # Tag an object
# ol_tag_object("de_results", "baseline_analysis")
# 
# # Label the entire project (tags all tables/objects)
# ol_label("experiment_v1")
# 
# # List tags
# ol_list_tags()
# 
# # List project labels
# ol_list_labels()

## ----multiple_versions--------------------------------------------------------
# # Change parameters and re‑run analysis
# de_params_v2 <- list(
#     method = "DESeq2",
#     alpha = 0.01,
#     lfc_threshold = 1.5
# )
# ol_save("de_params", de_params_v2)
# 
# # Recompute DE results with new parameters
# de_results_v2 <- data.frame(
#     gene_id = paste0("GENE", 1:10),
#     log2FC = rnorm(10, 0, 2.5),
#     pvalue = runif(10, 0, 0.01),
#     padj = runif(10, 0, 0.01)
# )
# ol_save("de_results", de_results_v2,
#     depends_on = c("normalized_counts", "de_params")
# )
# 
# # Tag the new version
# ol_tag_object("de_results", "strict_analysis")
# 
# # Try a different analysis method
# de_params_edger <- list(
#     method = "edgeR",
#     alpha = 0.05,
#     lfc_threshold = 1.0
# )
# ol_save("de_params", de_params_edger)
# 
# de_results_edger <- data.frame(
#     gene_id = paste0("GENE", c(1:12, 25:30)),
#     log2FC = rnorm(18, 0, 2),
#     pvalue = runif(18, 0, 0.1),
#     padj = runif(18, 0, 0.1)
# )
# ol_save("de_results", de_results_edger,
#     depends_on = c("normalized_counts", "de_params")
# )
# ol_tag_object("de_results", "edger_analysis")

## ----version_comparison-------------------------------------------------------
# # List all versions
# versions <- ol_list_object_versions("de_results")
# print(versions)
# 
# # Compare versions
# comparison <- ol_compare_versions("de_results")
# print(comparison)
# 
# # Compare specific tags
# tag_comparison <- ol_compare_versions("de_results",
#     versions = c("baseline_analysis", "strict_analysis")
# )
# print(tag_comparison)

## ----load_versions------------------------------------------------------------
# # Load by tag
# baseline_results <- ol_read_object(
#     "de_results",
#     ref = "@tag(baseline_analysis)"
# )
# strict_results <- ol_read_object("de_results", ref = "@tag(strict_analysis)")
# 
# cat("Baseline analysis:", nrow(baseline_results), "genes\n")
# cat("Strict analysis:", nrow(strict_results), "genes\n")

## ----commits------------------------------------------------------------------
# # Create a commit at a milestone
# commit_id <- ol_commit(
#     note = "Completed initial DE analysis with three methods",
#     params = list(
#         methods = c("DESeq2_baseline", "DESeq2_strict", "edgeR"),
#         date = as.character(Sys.Date())
#     )
# )
# 
# cat("Commit ID:", commit_id, "\n")

## ----history------------------------------------------------------------------
# # Show commit history
# commits <- ol_log_commits(n = 10)
# print(commits)
# 
# # History of a specific table
# table_log <- ol_log("raw_counts")
# print(table_log)

## ----visualization, fig.width=8, fig.height=6---------------------------------
# # The igraph package is required
# if (requireNamespace("igraph", quietly = TRUE)) {
#     # Visualize upstream dependencies
#     ol_plot_lineage("de_results",
#         direction = "upstream",
#         layout = "sugiyama",
#         main = "DE Results - Upstream Dependencies"
#     )
# 
#     # Visualize dependencies in both directions
#     ol_plot_lineage("normalized_counts",
#         direction = "both",
#         layout = "tree",
#         main = "Normalized Counts - Full Lineage"
#     )
# }

## ----checkout-----------------------------------------------------------------
# # Add a new table
# filtered_results <- de_results[de_results$padj < 0.05, ]
# ol_write("filtered_de", filtered_results)
# 
# # Revert to an earlier label state
# ol_checkout("experiment_v1")
# 
# # The filtered_de table will no longer exist
# current_tables <- ol_list_tables()
# cat(
#     "Tables after checkout:",
#     paste(current_tables$table_name, collapse = ", "),
#     "\n"
# )

## ----fread--------------------------------------------------------------------
# # Recreate raw_counts (may have been removed by checkout)
# ol_write("raw_counts", raw_counts, mode = "overwrite")
# 
# # Select specific columns and read
# selected <- ol_fread("raw_counts",
#     select = c("gene_id", "sample1", "sample2"),
#     nrows = 10
# )
# head(selected)
# 
# # Filtering by condition
# filtered <- ol_fread("raw_counts",
#     filter = "sample1 > 100"
# )
# head(filtered)

## ----lazy_eval----------------------------------------------------------------
# if (requireNamespace("dplyr", quietly = TRUE)) {
#     # collect = FALSE for lazy evaluation
#     lazy_tbl <- ol_read("raw_counts", collect = FALSE)
# 
#     # Process with dplyr
#     result <- lazy_tbl %>%
#         dplyr::filter(sample1 > 100) %>%
#         dplyr::select(gene_id, sample1) %>%
#         dplyr::collect()
# 
#     head(result)
# }

## ----drop---------------------------------------------------------------------
# # Remove an unnecessary table
# ol_drop("filtered_de")

## ----bioc_se------------------------------------------------------------------
# if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#     # Create long-format data
#     long_counts <- data.frame(
#         feature = rep(paste0("GENE", 1:100), each = 4),
#         sample = rep(paste0("sample", 1:4), times = 100),
#         value = rpois(400, 100)
#     )
#     ol_write("long_counts", long_counts, mode = "overwrite")
# 
#     # Load as a SummarizedExperiment
#     se <- ol_read_se("long_counts",
#         feature_col = "feature",
#         sample_col = "sample",
#         value_col = "value",
#         backing = "memory"
#     )
# 
#     print(se)
# }

## ----bioc_mae-----------------------------------------------------------------
# if (requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
#     # Prepare multiple assay datasets
#     rna_data <- data.frame(
#         feature = rep(paste0("GENE", 1:50), each = 4),
#         sample = rep(paste0("sample", 1:4), times = 50),
#         value = rpois(200, 100)
#     )
#     ol_write("rna_assay", rna_data, mode = "overwrite")
# 
#     protein_data <- data.frame(
#         feature = rep(paste0("PROT", 1:30), each = 4),
#         sample = rep(paste0("sample", 1:4), times = 30),
#         value = rnorm(120, 50, 10)
#     )
#     ol_write("protein_assay", protein_data, mode = "overwrite")
# 
#     # Load as a MultiAssayExperiment
#     mae <- ol_read_mae(
#         assays = list(
#             rna = list(name = "rna_assay"),
#             protein = list(name = "protein_assay")
#         ),
#         backing = "memory"
#     )
# 
#     print(mae)
# }

## ----complete_workflow--------------------------------------------------------
# # 1. Initialize the project
# ol_init("rnaseq_project")
# 
# # 2. Save raw data
# set.seed(456)
# raw_data <- data.frame(
#     gene_id = paste0("GENE", 1:200),
#     control_1 = rpois(200, 100),
#     control_2 = rpois(200, 100),
#     control_3 = rpois(200, 100),
#     treated_1 = rpois(200, 120),
#     treated_2 = rpois(200, 120),
#     treated_3 = rpois(200, 120)
# )
# ol_write("raw_expression", raw_data)
# 
# # 3. Save QC parameters
# qc_params <- list(
#     min_counts = 10,
#     min_samples = 3,
#     method = "standard"
# )
# ol_save("qc_parameters", qc_params)
# 
# # 4. Post-QC data
# qc_data <- raw_data[rowSums(raw_data[, -1] > 10) >= 3, ]
# ol_write(
#     "qc_filtered",
#     qc_data,
#     depends_on = c("raw_expression", "qc_parameters")
# )
# 
# # 5. Normalization parameters
# norm_params <- list(
#     method = "TMM",
#     log_transform = TRUE
# )
# ol_save("norm_parameters", norm_params)
# 
# # 6. Normalized data
# norm_data <- qc_data
# for (i in 2:ncol(norm_data)) {
#     norm_data[[i]] <- log2(norm_data[[i]] + 1)
# }
# ol_write("normalized", norm_data,
#     depends_on = c("qc_filtered", "norm_parameters")
# )
# 
# # 7. Statistical analysis parameters
# stats_params <- list(
#     test = "t-test",
#     alpha = 0.05,
#     correction = "BH"
# )
# ol_save("stats_parameters", stats_params)
# 
# # 8. Statistical analysis results
# set.seed(789)
# de_final <- data.frame(
#     gene_id = sample(qc_data$gene_id, 30),
#     log2fc = rnorm(30, 1, 0.5),
#     pvalue = runif(30, 0, 0.05),
#     padj = runif(30, 0, 0.05)
# )
# ol_save("final_de_results", de_final,
#     depends_on = c("normalized", "stats_parameters")
# )
# 
# # 9. Commit the analysis
# ol_commit(
#     note = "Complete RNA-seq differential expression analysis",
#     params = list(
#         samples = 6,
#         genes_tested = nrow(qc_data),
#         significant = nrow(de_final),
#         date = as.character(Sys.Date())
#     )
# )
# 
# # 10. Create a label
# ol_label("final_analysis_v1")
# 
# # 11. View the full lineage
# full_lineage <- ol_show_lineage("final_de_results", direction = "upstream")
# print(full_lineage)
# 
# # 12. Visualize dependencies
# if (requireNamespace("igraph", quietly = TRUE)) {
#     ol_plot_lineage("final_de_results",
#         direction = "upstream",
#         layout = "sugiyama",
#         main = "RNA-seq Analysis Pipeline"
#     )
# }

## ----session_info-------------------------------------------------------------
# sessionInfo()

