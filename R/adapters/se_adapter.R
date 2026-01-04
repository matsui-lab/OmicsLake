#' @title SummarizedExperiment Adapter
#' @description Adapter for storing and retrieving Bioconductor SummarizedExperiment objects.
#' Stores all components (assays, colData, rowData, metadata) with full fidelity.
#'
#' @details
#' This adapter decomposes a SummarizedExperiment into its components:
#' \itemize{
#'   \item Assays are stored as tables (sparse matrix -> long format)
#'   \item colData is stored as a table
#'   \item rowData is stored as a table
#'   \item metadata is stored as an R object
#' }
#'
#' @examples
#' \dontrun{
#' library(SummarizedExperiment)
#'
#' # Create a SE object
#' se <- SummarizedExperiment(
#'   assays = list(counts = matrix(1:100, 10, 10)),
#'   colData = DataFrame(sample = paste0("S", 1:10)),
#'   rowData = DataFrame(gene = paste0("G", 1:10))
#' )
#'
#' # Store in lake
#' lake$put("my_se", se)
#'
#' # Retrieve
#' se2 <- lake$get("my_se")
#' }
#'
#' @importFrom R6 R6Class
#' @export
SEAdapter <- R6::R6Class("SEAdapter",
  inherit = LakeAdapter,

  public = list(

    name = function() {
      "SummarizedExperiment"
    },

    can_handle = function(data) {
      inherits(data, "SummarizedExperiment") ||
        inherits(data, "RangedSummarizedExperiment")
    },

    priority = function() {
      100  # High priority for SE objects
    },

    put = function(lake, name, data) {
      if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("SummarizedExperiment package is required", call. = FALSE)
      }

      project <- lake$.__enclos_env__$private$.project
      state <- .ol_get_backend_state(project)
      conn <- state$conn
      prefix <- paste0(name, ".__se__.")

      # Collect component names for registry
      components <- c("colData", "rowData")

      # Prepare assay names
      assay_names <- SummarizedExperiment::assayNames(data)
      if (length(assay_names) == 0 && length(SummarizedExperiment::assays(data)) > 0) {
        assay_names <- paste0("assay", seq_along(SummarizedExperiment::assays(data)))
      }
      for (an in assay_names) {
        components <- c(components, paste0("assay.", an))
      }

      has_rowRanges <- inherits(data, "RangedSummarizedExperiment")
      if (has_rowRanges) {
        row_ranges <- SummarizedExperiment::rowRanges(data)
        if (length(row_ranges) > 0) {
          components <- c(components, "rowRanges")
        }
      }

      meta <- S4Vectors::metadata(data)
      if (length(meta) > 0) {
        components <- c(components, "metadata")
      }
      components <- c(components, "manifest")

      # Wrap all writes in a transaction for atomicity
      DBI::dbBegin(conn)
      tryCatch({
        # Store assays
        for (i in seq_along(assay_names)) {
          assay_name <- assay_names[i]
          mat <- SummarizedExperiment::assay(data, i)
          long_df <- private$.matrix_to_long(mat)
          ol_write(paste0(prefix, "assay.", assay_name), long_df,
                   project = project, mode = "overwrite")
        }

        # Store colData
        col_data <- as.data.frame(SummarizedExperiment::colData(data))
        col_data$.__sample_id__ <- rownames(col_data)
        if (nrow(col_data) == 0) {
          col_data <- data.frame(.__sample_id__ = colnames(data), stringsAsFactors = FALSE)
        }
        ol_write(paste0(prefix, "colData"), col_data, project = project, mode = "overwrite")

        # Store rowData
        row_data <- as.data.frame(SummarizedExperiment::rowData(data))
        row_data$.__feature_id__ <- rownames(row_data)
        if (nrow(row_data) == 0) {
          row_data <- data.frame(.__feature_id__ = rownames(data), stringsAsFactors = FALSE)
        }
        ol_write(paste0(prefix, "rowData"), row_data, project = project, mode = "overwrite")

        # Store rowRanges if present (as RangedSummarizedExperiment)
        if (has_rowRanges && length(row_ranges) > 0) {
          ranges_df <- as.data.frame(row_ranges)
          ranges_df$.__feature_id__ <- names(row_ranges)
          ol_write(paste0(prefix, "rowRanges"), ranges_df, project = project, mode = "overwrite")
        }

        # Store metadata
        if (length(meta) > 0) {
          ol_save(paste0(prefix, "metadata"), meta, project = project)
        }

        # Store manifest (internal object, not in adapter registry)
        manifest <- list(
          type = "SummarizedExperiment",
          class = class(data)[1],
          assay_names = assay_names,
          n_samples = ncol(data),
          n_features = nrow(data),
          has_rowRanges = has_rowRanges,
          has_metadata = length(meta) > 0,
          created_at = Sys.time()
        )
        ol_save(paste0(prefix, "manifest"), manifest, project = project)

        # Register in adapter table for deterministic lookup
        .ol_register_adapter_object(state, name, "SummarizedExperiment", components, format_version = 1L)

        DBI::dbCommit(conn)
      }, error = function(e) {
        DBI::dbRollback(conn)
        stop("SummarizedExperiment storage failed (rolled back): ", conditionMessage(e), call. = FALSE)
      })

      invisible(TRUE)
    },

    get = function(lake, name, ref = "@latest") {
      if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("SummarizedExperiment package is required", call. = FALSE)
      }

      prefix <- paste0(name, ".__se__.")
      project <- lake$.__enclos_env__$private$.project

      # Load manifest
      manifest <- tryCatch(
        ol_read_object(paste0(prefix, "manifest"), ref = ref, project = project),
        error = function(e) {
          stop("Cannot find SummarizedExperiment manifest for '", name, "'", call. = FALSE)
        }
      )

      # Load assays
      assays_list <- list()
      for (assay_name in manifest$assay_names) {
        long_df <- ol_read(paste0(prefix, "assay.", assay_name),
                          ref = ref, project = project)
        mat <- private$.long_to_matrix(long_df)
        assays_list[[assay_name]] <- mat
      }

      # Load colData
      col_data <- ol_read(paste0(prefix, "colData"), ref = ref, project = project)
      sample_ids <- col_data$.__sample_id__
      col_data$.__sample_id__ <- NULL
      rownames(col_data) <- sample_ids

      # Load rowData
      row_data <- ol_read(paste0(prefix, "rowData"), ref = ref, project = project)
      feature_ids <- row_data$.__feature_id__
      row_data$.__feature_id__ <- NULL
      rownames(row_data) <- feature_ids

      # Build SE
      se <- SummarizedExperiment::SummarizedExperiment(
        assays = assays_list,
        colData = S4Vectors::DataFrame(col_data),
        rowData = S4Vectors::DataFrame(row_data)
      )

      # Load metadata if present
      if (manifest$has_metadata) {
        meta <- tryCatch(
          ol_read_object(paste0(prefix, "metadata"), ref = ref, project = project),
          error = function(e) list()
        )
        S4Vectors::metadata(se) <- meta
      }

      se
    },

    components = function(lake, name) {
      prefix <- paste0(name, ".__se__.")
      tables <- lake$tables()
      objects <- lake$objects()

      # Find matching tables and objects
      matching_tables <- tables[grepl(paste0("^", gsub("\\.", "\\\\.", prefix)), tables$table_name), ]
      matching_objects <- objects[grepl(paste0("^", gsub("\\.", "\\\\.", prefix)), objects$name), ]

      components <- data.frame(
        component = c(matching_tables$table_name, matching_objects$name),
        type = c(rep("table", nrow(matching_tables)), rep("object", nrow(matching_objects))),
        stringsAsFactors = FALSE
      )

      # Clean up component names
      components$component <- gsub(paste0("^", gsub("\\.", "\\\\.", prefix)), "", components$component)

      components
    },

    exists = function(lake, name) {
      prefix <- paste0(name, ".__se__.")
      objects <- tryCatch(lake$objects(), error = function(e) data.frame(name = character(0)))
      manifest_name <- paste0(prefix, "manifest")
      manifest_name %in% objects$name
    }
  ),

  private = list(

    # Convert matrix to long format data frame
    .matrix_to_long = function(mat) {
      if (is.null(rownames(mat))) {
        rownames(mat) <- paste0("row", seq_len(nrow(mat)))
      }
      if (is.null(colnames(mat))) {
        colnames(mat) <- paste0("col", seq_len(ncol(mat)))
      }

      # Handle sparse matrices
      if (inherits(mat, "sparseMatrix")) {
        if (!requireNamespace("Matrix", quietly = TRUE)) {
          stop("Matrix package required for sparse matrices", call. = FALSE)
        }
        # Use Matrix::summary for sparse matrices (only non-zero values)
        summ <- Matrix::summary(mat)
        data.frame(
          feature = rownames(mat)[summ$i],
          sample = colnames(mat)[summ$j],
          value = summ$x,
          stringsAsFactors = FALSE
        )
      } else {
        # Dense matrix
        long_df <- as.data.frame(as.table(as.matrix(mat)))
        names(long_df) <- c("feature", "sample", "value")
        long_df$feature <- as.character(long_df$feature)
        long_df$sample <- as.character(long_df$sample)
        long_df$value <- as.numeric(long_df$value)
        # Remove zeros for efficiency
        long_df <- long_df[long_df$value != 0, ]
        long_df
      }
    },

    # Convert long format back to matrix
    .long_to_matrix = function(long_df) {
      if (nrow(long_df) == 0) {
        return(matrix(0, 0, 0))
      }

      features <- unique(long_df$feature)
      samples <- unique(long_df$sample)

      # Create sparse matrix
      if (requireNamespace("Matrix", quietly = TRUE)) {
        mat <- Matrix::sparseMatrix(
          i = match(long_df$feature, features),
          j = match(long_df$sample, samples),
          x = long_df$value,
          dims = c(length(features), length(samples)),
          dimnames = list(features, samples)
        )
      } else {
        # Fall back to dense matrix
        mat <- matrix(0, length(features), length(samples),
                      dimnames = list(features, samples))
        for (k in seq_len(nrow(long_df))) {
          mat[long_df$feature[k], long_df$sample[k]] <- long_df$value[k]
        }
      }

      mat
    }
  )
)

# Register the SE adapter on package load
.onLoad_adapters <- function() {
  register_adapter(SEAdapter$new())
}
