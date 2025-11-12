#!/usr/bin/env Rscript
# Generate benchmark datasets for OmicsLake paper
# This script creates synthetic datasets for performance benchmarking

library(arrow)

cat("Generating benchmark datasets...\n")

# Set seed for reproducibility
set.seed(42)

# Create output directory
# Use relative path to work correctly when sourced from inst/paper/ directory
output_dir <- "benchmark_datasets"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# --- 1. Generate 100MB Parquet dataset ---
cat("Creating 100MB parquet dataset...\n")
n_rows <- 1e6
df_100mb <- data.frame(
  id = 1:n_rows,
  gene_id = paste0("gene_", sample(1:20000, n_rows, replace = TRUE)),
  sample_id = paste0("sample_", sample(1:100, n_rows, replace = TRUE)),
  expression = rnorm(n_rows, mean = 50, sd = 20),
  pvalue = runif(n_rows),
  log2fc = rnorm(n_rows, mean = 0, sd = 2),
  category = sample(c("up", "down", "unchanged"), n_rows, replace = TRUE),
  annotation = sample(c("protein_coding", "lncRNA", "miRNA", "pseudogene"), n_rows, replace = TRUE),
  chromosome = paste0("chr", sample(1:22, n_rows, replace = TRUE)),
  start_pos = sample(1:250000000, n_rows, replace = TRUE),
  stringsAsFactors = FALSE
)

# Write as Parquet
arrow::write_parquet(df_100mb, file.path(output_dir, "dataset_100MB.parquet"))

# Also save as RDS for base R comparison
saveRDS(df_100mb, file.path(output_dir, "rds_100MB.RDS"))

cat("  - dataset_100MB.parquet created\n")
cat("  - rds_100MB.RDS created\n")

# --- 2. Generate 1M row tables for join benchmark ---
cat("Creating 1M row tables for join benchmark...\n")
n_join <- 1e6

table1 <- data.frame(
  id = 1:n_join,
  gene_name = paste0("gene_", 1:n_join),
  expression = rnorm(n_join, mean = 100, sd = 30),
  condition = sample(c("control", "treatment"), n_join, replace = TRUE),
  stringsAsFactors = FALSE
)

table2 <- data.frame(
  id = sample(1:n_join, n_join, replace = FALSE),  # Shuffled IDs for realistic join
  annotation = sample(c("pathway_A", "pathway_B", "pathway_C", "pathway_D"), n_join, replace = TRUE),
  go_term = paste0("GO:", sample(1000:9999, n_join, replace = TRUE)),
  protein_length = sample(50:2000, n_join, replace = TRUE),
  stringsAsFactors = FALSE
)

write.csv(table1, file.path(output_dir, "table1_1M.csv"), row.names = FALSE)
write.csv(table2, file.path(output_dir, "table2_1M.csv"), row.names = FALSE)

cat("  - table1_1M.csv created\n")
cat("  - table2_1M.csv created\n")

# --- 3. Generate mock RNA-seq data ---
cat("Creating mock RNA-seq data...\n")
n_genes <- 20000
n_samples <- 6

raw_counts <- matrix(
  rpois(n_genes * n_samples, lambda = 50),
  nrow = n_genes,
  ncol = n_samples
)

colnames(raw_counts) <- paste0("sample", 1:n_samples)
rownames(raw_counts) <- paste0("gene", 1:n_genes)

rnaseq_mock_data <- list(
  counts = raw_counts,
  gene_info = data.frame(
    gene_id = paste0("gene", 1:n_genes),
    gene_name = paste0("GENE", 1:n_genes),
    chromosome = paste0("chr", sample(1:22, n_genes, replace = TRUE)),
    stringsAsFactors = FALSE
  ),
  sample_info = data.frame(
    sample_id = paste0("sample", 1:n_samples),
    condition = rep(c("control", "treatment"), each = 3),
    batch = rep(c("batch1", "batch2"), times = 3),
    stringsAsFactors = FALSE
  )
)

save(rnaseq_mock_data, file = file.path(output_dir, "rnaseq_mock_data.RData"))

cat("  - rnaseq_mock_data.RData created\n")

cat("\nAll benchmark datasets generated successfully!\n")
cat("Location:", normalizePath(output_dir), "\n")
