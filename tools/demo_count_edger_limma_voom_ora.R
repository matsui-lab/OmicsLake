#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(edgeR)
  library(limma)
  library(org.Hs.eg.db)
  library(AnnotationDbi)
})

if (!requireNamespace("OmicsLake", quietly = TRUE)) {
  stop("OmicsLake package is required.", call. = FALSE)
}

if (!exists("track_pipeline", where = asNamespace("OmicsLake"), inherits = FALSE)) {
  if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
    suppressPackageStartupMessages(devtools::load_all(".", quiet = TRUE))
  }
}

if (!exists("track_pipeline", where = asNamespace("OmicsLake"), inherits = FALSE)) {
  stop("track_pipeline() is unavailable. Install or load a recent OmicsLake build.", call. = FALSE)
}

set.seed(42)

# Demo workspace
demo_root <- file.path(tempdir(), "omicslake_demo_root")
demo_data_dir <- file.path(tempdir(), "omicslake_demo_data")
dir.create(demo_root, recursive = TRUE, showWarnings = FALSE)
dir.create(demo_data_dir, recursive = TRUE, showWarnings = FALSE)
options(ol.root = demo_root)

project <- paste0("demo_count_edger_limma_voom_ora_", format(Sys.time(), "%Y%m%d_%H%M%S"))
OmicsLake::use_lake(project)

# 1) Simulate count + metadata files
n_genes <- 3000
n_samples <- 6
groups <- factor(rep(c("Control", "Treat"), each = n_samples / 2))
sample_ids <- paste0("S", seq_len(n_samples))

entrez_pool <- AnnotationDbi::keys(org.Hs.eg.db, keytype = "ENTREZID")
gene_ids <- sample(entrez_pool, n_genes)

base_mu <- rgamma(n_genes, shape = 2, scale = 40)
de_idx <- sample(seq_len(n_genes), 300)
fold_change <- rep(1, n_genes)
fold_change[de_idx] <- 2

counts <- matrix(0L, nrow = n_genes, ncol = n_samples)
for (j in seq_len(n_samples)) {
  mu <- base_mu
  if (groups[j] == "Treat") {
    mu <- mu * fold_change
  }
  counts[, j] <- rnbinom(n_genes, mu = mu, size = 20)
}
rownames(counts) <- gene_ids
colnames(counts) <- sample_ids

meta <- data.frame(sample = sample_ids, group = groups, stringsAsFactors = FALSE)

counts_file <- file.path(demo_data_dir, "counts.tsv")
meta_file <- file.path(demo_data_dir, "sample_meta.tsv")
deg_file <- file.path(demo_data_dir, "deg_limma_voom.csv")
ora_file <- file.path(demo_data_dir, "ora_go_bp_goana.csv")

write.table(counts, counts_file, sep = "\t", quote = FALSE, col.names = NA)
write.table(meta, meta_file, sep = "\t", quote = FALSE, row.names = FALSE)

# 2) count -> edgeR -> limma-voom -> ORA (goana)
summary_result <- OmicsLake::track_pipeline({
  counts_in <- as.matrix(read.delim(counts_file, row.names = 1, check.names = FALSE))
  meta_in <- read.delim(meta_file, stringsAsFactors = FALSE)
  group <- factor(meta_in$group, levels = c("Control", "Treat"))

  design <- model.matrix(~ 0 + group)
  colnames(design) <- levels(group)

  y <- edgeR::DGEList(counts = counts_in, group = group)
  keep <- edgeR::filterByExpr(y, design = design)
  y <- y[keep, , keep.lib.sizes = FALSE]
  y <- edgeR::calcNormFactors(y)

  v <- limma::voom(y, design, plot = FALSE)
  contrast <- limma::makeContrasts(Treat - Control, levels = design)
  fit <- limma::lmFit(v, design)
  fit <- limma::contrasts.fit(fit, contrast)
  fit <- limma::eBayes(fit)

  deg <- limma::topTable(fit, number = Inf, sort.by = "P")
  deg$ENTREZID <- rownames(deg)
  write.csv(deg, deg_file, row.names = FALSE)

  ora <- limma::goana(fit, coef = 1, species = "Hs")
  ora$GOID <- rownames(ora)
  ora <- ora[order(ora$P.Up), ]
  ora_top <- utils::head(ora, 20)
  write.csv(ora_top, ora_file, row.names = FALSE)

  list(
    n_input_genes = nrow(counts_in),
    n_filtered_genes = nrow(y$counts),
    n_sig_deg = sum(deg$adj.P.Val < 0.05, na.rm = TRUE),
    n_ora_terms = nrow(ora_top)
  )
},
snapshot = "demo_v1",
save_result = TRUE,
result_name = "run_summary",
result_depends_on = "writes",
store_observation = TRUE,
observation_name = "obs_run",
observation_depends_on = "both"
)

run_summary <- OmicsLake::fetch("run_summary")
obs_run <- OmicsLake::fetch("obs_run")
deps_summary <- OmicsLake::lake()$deps("run_summary", direction = "up")

cat("\n=== Demo Completed ===\n")
cat("Project:", project, "\n")
cat("Lake root:", demo_root, "\n")
cat("Data dir:", demo_data_dir, "\n\n")

cat("Run summary:\n")
print(run_summary)

cat("\nObservation record keys:\n")
print(names(obs_run))

cat("\nUpstream dependencies of run_summary (first 10):\n")
dep_cols <- intersect(c("parent_name", "child_name", "parent", "child"), names(deps_summary))
if (length(dep_cols) > 0) {
  print(utils::head(deps_summary[, dep_cols, drop = FALSE], 10))
} else {
  print(utils::head(deps_summary, 10))
}

cat("\nOutput files:\n")
cat(" -", deg_file, "\n")
cat(" -", ora_file, "\n")
cat("\nYou can inspect lineage with: OmicsLake::lake()$tree(\"run_summary\")\n")
