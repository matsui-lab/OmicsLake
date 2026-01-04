#' @title OmicsLake Evaluation Data Generators
#' @description Functions for generating synthetic datasets for benchmarking
#' @name eval_generate
NULL

#' Generate a synthetic table for benchmarking
#'
#' Creates a data frame with specified dimensions containing numeric and
#' optionally mixed-type data suitable for benchmarking I/O, filtering,
#' and aggregation operations.
#'
#' @param n_rows Number of rows to generate
#' @param n_cols Number of columns (excluding id column)
#' @param seed Random seed for reproducibility
#' @param type Data type: "numeric" (all numeric) or "mixed" (includes categorical)
#' @param na_rate Rate of NA values to introduce (0-1)
#' @param id_col Whether to include an id column
#' @return A data frame with the specified dimensions
#' @export
#' @examples
#' \dontrun{
#' df <- ol_eval_generate_table(100000, 20, seed = 42)
#' }
ol_eval_generate_table <- function(n_rows, n_cols, seed = 1,
                                   type = c("numeric", "mixed"),
                                   na_rate = 0.01, id_col = TRUE) {
  type <- match.arg(type)
  set.seed(seed)

  # Pre-allocate list for columns
  cols <- vector("list", n_cols + if (id_col) 1 else 0)
  col_names <- character(length(cols))

  idx <- 1

  # Add id column if requested
  if (id_col) {
    cols[[idx]] <- seq_len(n_rows)
    col_names[idx] <- "id"
    idx <- idx + 1
  }

  if (type == "numeric") {
    # All numeric columns
    for (i in seq_len(n_cols)) {
      # Vary distribution types for realistic data
      dist_type <- i %% 3
      col_data <- switch(
        as.character(dist_type),
        "0" = rnorm(n_rows, mean = 50, sd = 20),          # Normal
        "1" = runif(n_rows, min = 0, max = 100),          # Uniform
        "2" = rexp(n_rows, rate = 0.1)                    # Exponential
      )

      # Introduce NAs
      if (na_rate > 0) {
        na_idx <- sample(n_rows, size = ceiling(n_rows * na_rate))
        col_data[na_idx] <- NA
      }

      cols[[idx]] <- col_data
      col_names[idx] <- paste0("x", i)
      idx <- idx + 1
    }
  } else {
    # Mixed type: 70% numeric, 20% categorical, 10% character
    n_numeric <- ceiling(n_cols * 0.7)
    n_categorical <- ceiling(n_cols * 0.2)
    n_character <- n_cols - n_numeric - n_categorical

    # Numeric columns
    for (i in seq_len(n_numeric)) {
      cols[[idx]] <- rnorm(n_rows, mean = 100, sd = 30)
      col_names[idx] <- paste0("num", i)
      idx <- idx + 1
    }

    # Categorical columns
    categories <- list(
      c("control", "treatment", "reference"),
      c("batch1", "batch2", "batch3", "batch4"),
      c("low", "medium", "high"),
      c("A", "B", "C", "D", "E")
    )

    for (i in seq_len(n_categorical)) {
      cat_opts <- categories[[(i - 1) %% length(categories) + 1]]
      cols[[idx]] <- sample(cat_opts, n_rows, replace = TRUE)
      col_names[idx] <- paste0("cat", i)
      idx <- idx + 1
    }

    # Character columns (e.g., gene IDs)
    for (i in seq_len(max(0, n_character))) {
      cols[[idx]] <- paste0("item_", sample.int(n_rows * 10, n_rows, replace = TRUE))
      col_names[idx] <- paste0("str", i)
      idx <- idx + 1
    }
  }

  # Build data frame
  names(cols) <- col_names
  as.data.frame(cols, stringsAsFactors = FALSE)
}

#' Generate a dimension table for join benchmarks
#'
#' Creates a smaller "dimension" table suitable for join operations,
#' with a foreign key relationship to a main table.
#'
#' @param n_rows Number of rows (unique keys)
#' @param seed Random seed for reproducibility
#' @param key_col Name of the key column
#' @return A data frame with dimension data
#' @export
ol_eval_generate_dim_table <- function(n_rows, seed = 1, key_col = "id") {
  set.seed(seed)

  df <- data.frame(
    .key = seq_len(n_rows),
    category = sample(c("pathway_A", "pathway_B", "pathway_C", "pathway_D"),
                      n_rows, replace = TRUE),
    annotation = paste0("GO:", sprintf("%04d", sample.int(9999, n_rows, replace = TRUE))),
    score = runif(n_rows, 0, 100),
    priority = sample(1:5, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Rename key column to respect key_col parameter

  names(df)[names(df) == ".key"] <- key_col
  df
}

#' Generate synthetic RNA-seq data for case study
#'
#' Creates minimal RNA-seq-like data structures for the W3 case study,
#' including counts matrix, gene metadata, and sample metadata.
#'
#' @param seed Random seed for reproducibility
#' @param n_genes Number of genes
#' @param n_samples Number of samples
#' @return List with counts, gene_info, and sample_info data frames
#' @export
#' @examples
#' \dontrun{
#' rnaseq <- ol_eval_generate_case_rnaseq(seed = 42, n_genes = 20000, n_samples = 6)
#' }
ol_eval_generate_case_rnaseq <- function(seed = 1, n_genes = 20000, n_samples = 6) {
  set.seed(seed)

  # Generate count matrix with realistic RNA-seq characteristics
  # - Most genes low expression
  # - Some highly expressed
  # - Zero-inflation

  # Base expression levels (log-scale)
  base_expr <- rnorm(n_genes, mean = 3, sd = 2)
  base_expr <- pmax(0, base_expr)  # Ensure non-negative

  # Generate counts for each sample
  counts <- matrix(0, nrow = n_genes, ncol = n_samples)

  for (j in seq_len(n_samples)) {
    # Sample-specific variation
    sample_effect <- rnorm(1, mean = 1, sd = 0.2)

    for (i in seq_len(n_genes)) {
      # Negative binomial for realistic count distribution
      lambda <- exp(base_expr[i]) * sample_effect
      counts[i, j] <- stats::rpois(1, lambda = lambda)
    }
  }

  # Add gene names
  rownames(counts) <- paste0("ENSG", sprintf("%011d", seq_len(n_genes)))
  colnames(counts) <- paste0("sample", seq_len(n_samples))

  # Convert to data frame for OmicsLake storage
  counts_df <- as.data.frame(counts)
  counts_df$gene_id <- rownames(counts)
  counts_df <- counts_df[, c("gene_id", colnames(counts))]

  # Gene metadata
  chromosomes <- c(paste0("chr", 1:22), "chrX", "chrY")
  gene_info <- data.frame(
    gene_id = rownames(counts),
    gene_name = paste0("GENE", seq_len(n_genes)),
    gene_type = sample(c("protein_coding", "lncRNA", "miRNA", "pseudogene"),
                       n_genes, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1)),
    chromosome = sample(chromosomes, n_genes, replace = TRUE),
    start_pos = sample.int(250000000, n_genes, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Sample metadata
  # Use length.out to handle odd sample counts correctly
  conditions <- rep(c("control", "treatment"), length.out = n_samples)

  sample_info <- data.frame(
    sample_id = paste0("sample", seq_len(n_samples)),
    condition = conditions,
    batch = rep(c("batch1", "batch2"), length.out = n_samples),
    quality_score = runif(n_samples, 0.8, 1.0),
    stringsAsFactors = FALSE
  )

  list(
    counts = counts_df,
    gene_info = gene_info,
    sample_info = sample_info
  )
}

#' Generate a list object for object storage benchmarks
#'
#' @param size Approximate target size: "small", "medium", or "large"
#' @param seed Random seed
#' @return A complex list suitable for object serialization benchmarks
#' @export
ol_eval_generate_object <- function(size = c("small", "medium", "large"), seed = 1) {
  size <- match.arg(size)
  set.seed(seed)

  # Determine dimensions based on size
  n <- switch(size,
    small = 1000,
    medium = 10000,
    large = 100000
  )

  list(
    metadata = list(
      created = Sys.time(),
      description = "Benchmark object",
      version = "1.0"
    ),
    numeric_vector = rnorm(n),
    matrix = matrix(rnorm(n * 10), nrow = n, ncol = 10),
    data_frame = data.frame(
      id = seq_len(n),
      value = rnorm(n),
      category = sample(letters[1:5], n, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    nested = list(
      a = rnorm(n / 10),
      b = list(x = 1:10, y = letters),
      c = matrix(1:(n/10), nrow = n/100)
    )
  )
}

#' Generate annotation table for multi-table joins
#'
#' @param n_rows Number of annotation entries
#' @param seed Random seed
#' @return Data frame with annotation data
#' @export
ol_eval_generate_annotation <- function(n_rows, seed = 1) {
  set.seed(seed)

  data.frame(
    annotation_id = seq_len(n_rows),
    term = paste0("TERM:", sprintf("%06d", seq_len(n_rows))),
    description = paste("Description for term", seq_len(n_rows)),
    category = sample(c("biological_process", "molecular_function", "cellular_component"),
                      n_rows, replace = TRUE),
    level = sample(1:10, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}
