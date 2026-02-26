#' @title SummarizedExperiment Adapter
#' @description Adapter for storing and retrieving Bioconductor
#' SummarizedExperiment objects.
#' Stores all components (assays, colData, rowData, metadata) with full
#' fidelity.
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
#' if (FALSE) {
#'     library(SummarizedExperiment)
#'
#'     # Create a SE object
#'     se <- SummarizedExperiment(
#'         assays = list(counts = matrix(1:100, 10, 10)),
#'         colData = DataFrame(sample = paste0("S", 1:10)),
#'         rowData = DataFrame(gene = paste0("G", 1:10))
#'     )
#'
#'     # Store in lake
#'     lake$put("my_se", se)
#'
#'     # Retrieve
#'     se2 <- lake$get("my_se")
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
            100 # High priority for SE objects
        },
        put = function(lake, name, data) {
            if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
                stop("SummarizedExperiment package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__se__.")
            project <- lake$.__enclos_env__$private$.project
            assay_names <- private$.put_assays(prefix, data, project)
            col_data_mode <- private$.put_col_data(prefix, data, project)
            row_data_mode <- private$.put_row_data(prefix, data, project)
            row_ranges_mode <- private$.put_row_ranges(prefix, data, project)
            has_meta <- private$.put_metadata(prefix, data, project)
            private$.put_manifest(prefix, data, assay_names, row_ranges_mode,
                has_meta, col_data_mode, row_data_mode, project)
            invisible(TRUE)
        },
        get = function(lake, name, ref = "@latest") {
            if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
                stop("SummarizedExperiment package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__se__.")
            project <- lake$.__enclos_env__$private$.project
            manifest <- private$.load_manifest(prefix, name, ref, project)
            assays_list <- private$.load_assays(prefix, manifest$assay_names,
                ref, project, feature_ids = manifest$feature_ids,
                sample_ids = manifest$sample_ids)
            col_data_mode <- private$.manifest_slot_mode(manifest,
                "col_data_mode")
            row_data_mode <- private$.manifest_slot_mode(manifest,
                "row_data_mode")
            col_data <- private$.load_col_data(prefix, ref, project,
                sample_ids = manifest$sample_ids,
                mode = col_data_mode)
            row_data <- private$.load_row_data(prefix, ref, project,
                feature_ids = manifest$feature_ids,
                mode = row_data_mode)
            row_ranges_mode <- if (isTRUE(manifest$has_rowRanges)) {
                private$.manifest_slot_mode(manifest, "row_ranges_mode")
            } else {
                "none"
            }
            row_ranges <- NULL
            if (!identical(row_ranges_mode, "none")) {
                row_ranges <- private$.load_row_ranges(prefix, ref, project,
                    mode = row_ranges_mode,
                    feature_ids = manifest$feature_ids)
            }
            if (!is.null(row_ranges)) {
                se <- SummarizedExperiment::SummarizedExperiment(
                    assays = assays_list,
                    colData = S4Vectors::DataFrame(col_data),
                    rowRanges = row_ranges
                )
            } else {
                se <- SummarizedExperiment::SummarizedExperiment(
                    assays = assays_list,
                    colData = S4Vectors::DataFrame(col_data),
                    rowData = S4Vectors::DataFrame(row_data)
                )
            }
            if (manifest$has_metadata) {
                S4Vectors::metadata(se) <- private$.load_metadata(prefix, ref,
                    project)
            }
            se
        },
        components = function(lake, name) {
            prefix <- paste0(name, ".__se__.")
            prefix_pattern <- paste0("^", gsub(
                "([.|()\\^{}+$*?\\[\\]\\\\])",
                "\\\\\\1",
                prefix,
                perl = TRUE
            ))
            tables <- lake$tables()
            objects <- lake$objects()

            # Find matching tables and objects
            matching_tables <- tables[grepl(prefix_pattern, tables$table_name),
                , drop = FALSE]
            matching_objects <- objects[grepl(prefix_pattern, objects$name),
                , drop = FALSE]

            components <- data.frame(
                component = c(matching_tables$table_name,
                    matching_objects$name),
                type = c(rep("table", nrow(matching_tables)), rep("object",
                    nrow(matching_objects))),
                stringsAsFactors = FALSE
            )

            # User-friendly component ids without the internal prefix
            components$component_id <- gsub(prefix_pattern, "",
                components$component)

            components
        },
        exists = function(lake, name) {
            prefix <- paste0(name, ".__se__.")
            objects <- tryCatch(lake$objects(),
                error = function(e) data.frame(name = character(0)))
            manifest_name <- paste0(prefix, "manifest")
            manifest_name %in% objects$name
        },
        list_names = function(lake) {
            objects <- tryCatch(
                lake$objects(),
                error = function(e) data.frame(name = character(0))
            )
            if (!("name" %in% names(objects)) || nrow(objects) == 0) {
                return(character(0))
            }
            manifest_pattern <- "\\.__se__\\.manifest$"
            manifests <- objects$name[grepl(manifest_pattern, objects$name)]
            if (!length(manifests)) {
                return(character(0))
            }
            roots <- sort(unique(sub(manifest_pattern, "", manifests)))
            roots[!grepl("\\.__(se|sce|mae|spectra|qfeatures|mse|xcms|chrom|seurat)__\\.",
                roots,
                perl = TRUE)]
        }
    ),
    private = list(
        .put_assays = function(prefix, data, project) {
            assay_names <- SummarizedExperiment::assayNames(data)
            if (length(assay_names) == 0 && 
                length(SummarizedExperiment::assays(data)) > 0) {
                assay_names <- paste0("assay",
                    seq_along(SummarizedExperiment::assays(data)))
            }
            for (i in seq_along(assay_names)) {
                assay_name <- assay_names[i]
                mat <- SummarizedExperiment::assay(data, i)
                long_df <- private$.matrix_to_long(mat)
                ol_write(
                    paste0(prefix, "assay.", assay_name),
                    long_df,
                    project = project,
                    mode = "overwrite"
                )
            }
            assay_names
        },
        .put_col_data = function(prefix, data, project) {
            col_data <- as.data.frame(SummarizedExperiment::colData(data))
            sample_ids <- if (!is.null(colnames(data))) {
                colnames(data)
            } else {
                paste0("col", seq_len(ncol(data)))
            }
            if (nrow(col_data) == 0) {
                col_data <- data.frame(
                    .__sample_id__ = sample_ids,
                    stringsAsFactors = FALSE
                )
            } else {
                col_data$.__sample_id__ <- as.character(sample_ids)
            }
            if (any(vapply(col_data, is.list, logical(1)))) {
                ol_save(paste0(prefix, "colData.object"), col_data,
                    project = project)
                return("object")
            }
            ol_write(
                paste0(prefix, "colData"),
                col_data,
                project = project,
                mode = "overwrite"
            )
            "table"
        },
        .put_row_data = function(prefix, data, project) {
            row_data <- as.data.frame(SummarizedExperiment::rowData(data))
            feature_ids <- if (!is.null(rownames(data))) {
                rownames(data)
            } else {
                paste0("row", seq_len(nrow(data)))
            }
            if (nrow(row_data) == 0) {
                row_data <- data.frame(
                    .__feature_id__ = feature_ids,
                    stringsAsFactors = FALSE
                )
            } else {
                row_data$.__feature_id__ <- as.character(feature_ids)
            }
            if (any(vapply(row_data, is.list, logical(1)))) {
                ol_save(paste0(prefix, "rowData.object"), row_data,
                    project = project)
                return("object")
            }
            ol_write(
                paste0(prefix, "rowData"),
                row_data,
                project = project,
                mode = "overwrite"
            )
            "table"
        },
        .put_row_ranges = function(prefix, data, project) {
            if (!inherits(data, "RangedSummarizedExperiment")) {
                return("none")
            }
            row_ranges <- SummarizedExperiment::rowRanges(data)
            if (length(row_ranges) == 0) {
                return("none")
            }
            ranges_df <- tryCatch(
                as.data.frame(row_ranges),
                error = function(e) NULL
            )
            if (is.null(ranges_df) || nrow(ranges_df) != length(row_ranges)) {
                ol_save(paste0(prefix, "rowRanges.object"), row_ranges,
                    project = project)
                return("object")
            }
            feature_ids <- names(row_ranges)
            if (is.null(feature_ids) || length(feature_ids) != nrow(ranges_df)) {
                feature_ids <- rownames(data)
            }
            if (is.null(feature_ids) || length(feature_ids) != nrow(ranges_df)) {
                feature_ids <- paste0("row", seq_len(nrow(ranges_df)))
            }
            ranges_df$.__feature_id__ <- as.character(feature_ids)
            ol_write(
                paste0(prefix, "rowRanges"),
                ranges_df,
                project = project,
                mode = "overwrite"
            )
            "table"
        },
        .put_metadata = function(prefix, data, project) {
            meta <- S4Vectors::metadata(data)
            if (length(meta) > 0) {
                ol_save(paste0(prefix, "metadata"), meta, project = project)
            }
            isTRUE(length(meta) > 0)
        },
        .put_manifest = function(prefix, data, assay_names, row_ranges_mode,
            has_meta, col_data_mode, row_data_mode, project) {
            manifest <- list(
                type = "SummarizedExperiment",
                class = class(data)[1],
                assay_names = assay_names,
                n_samples = ncol(data),
                n_features = nrow(data),
                sample_ids = if (!is.null(colnames(data))) {
                    colnames(data)
                } else {
                    paste0("col", seq_len(ncol(data)))
                },
                feature_ids = if (!is.null(rownames(data))) {
                    rownames(data)
                } else {
                    paste0("row", seq_len(nrow(data)))
                },
                has_rowRanges = !identical(row_ranges_mode, "none"),
                row_ranges_mode = as.character(row_ranges_mode),
                has_metadata = isTRUE(has_meta),
                col_data_mode = as.character(col_data_mode),
                row_data_mode = as.character(row_data_mode),
                created_at = Sys.time()
            )
            ol_save(paste0(prefix, "manifest"), manifest, project = project)
            invisible(TRUE)
        },
        .load_manifest = function(prefix, name, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "manifest"), ref = ref,
                    project = project),
                error = function(e) {
                    stop(
                        "Cannot find SummarizedExperiment manifest for '",
                        name,
                        "'",
                        call. = FALSE
                    )
                }
            )
        },
        .load_assays = function(prefix, assay_names, ref, project,
            feature_ids = NULL, sample_ids = NULL) {
            assays_list <- list()
            for (assay_name in assay_names) {
                long_df <- ol_read(
                    paste0(prefix, "assay.", assay_name),
                    ref = ref,
                    project = project
                )
                assays_list[[assay_name]] <- private$.long_to_matrix(
                    long_df,
                    feature_ids = feature_ids,
                    sample_ids = sample_ids
                )
            }
            assays_list
        },
        .load_col_data = function(prefix, ref, project, sample_ids = NULL,
            mode = "table") {
            col_data <- if (identical(mode, "object")) {
                ol_read_object(paste0(prefix, "colData.object"), ref = ref,
                    project = project)
            } else {
                ol_read(paste0(prefix, "colData"), ref = ref,
                    project = project)
            }
            ids <- if (".__sample_id__" %in% names(col_data)) {
                as.character(col_data$.__sample_id__)
            } else {
                NULL
            }
            if (".__sample_id__" %in% names(col_data)) {
                col_data$.__sample_id__ <- NULL
            }
            if (is.null(ids) || length(ids) != nrow(col_data)) {
                ids <- sample_ids
            }
            if (is.null(ids) || length(ids) != nrow(col_data)) {
                ids <- paste0("col", seq_len(nrow(col_data)))
            }
            if (!is.null(sample_ids) &&
                length(sample_ids) == nrow(col_data) &&
                all(sample_ids %in% ids)) {
                ord <- match(sample_ids, ids)
                col_data <- col_data[ord, , drop = FALSE]
                ids <- as.character(sample_ids)
            }
            rownames(col_data) <- ids
            col_data
        },
        .load_row_data = function(prefix, ref, project, feature_ids = NULL,
            mode = "table") {
            row_data <- if (identical(mode, "object")) {
                ol_read_object(paste0(prefix, "rowData.object"), ref = ref,
                    project = project)
            } else {
                ol_read(paste0(prefix, "rowData"), ref = ref,
                    project = project)
            }
            ids <- if (".__feature_id__" %in% names(row_data)) {
                as.character(row_data$.__feature_id__)
            } else {
                NULL
            }
            if (".__feature_id__" %in% names(row_data)) {
                row_data$.__feature_id__ <- NULL
            }
            if (is.null(ids) || length(ids) != nrow(row_data)) {
                ids <- feature_ids
            }
            if (is.null(ids) || length(ids) != nrow(row_data)) {
                ids <- paste0("row", seq_len(nrow(row_data)))
            }
            if (!is.null(feature_ids) &&
                length(feature_ids) == nrow(row_data) &&
                all(feature_ids %in% ids)) {
                ord <- match(feature_ids, ids)
                row_data <- row_data[ord, , drop = FALSE]
                ids <- as.character(feature_ids)
            }
            rownames(row_data) <- ids
            row_data
        },
        .load_row_ranges = function(prefix, ref, project, mode = "table",
            feature_ids = NULL) {
            if (identical(mode, "object")) {
                rr <- tryCatch(
                    ol_read_object(paste0(prefix, "rowRanges.object"),
                        ref = ref, project = project),
                    error = function(e) NULL
                )
                if (is.null(rr) || is.null(feature_ids) || length(feature_ids) == 0) {
                    return(rr)
                }
                rn <- names(rr)
                if (!is.null(rn) && all(feature_ids %in% rn)) {
                    rr <- rr[match(feature_ids, rn)]
                }
                return(rr)
            }

            ranges_df <- tryCatch(
                ol_read(paste0(prefix, "rowRanges"), ref = ref,
                    project = project),
                error = function(e) NULL
            )
            if (is.null(ranges_df) || nrow(ranges_df) == 0) {
                return(NULL)
            }
            if (!requireNamespace("GenomicRanges", quietly = TRUE)) {
                return(NULL)
            }
            feature_ids <- ranges_df$.__feature_id__
            ranges_df$.__feature_id__ <- NULL
            seqname_fields <- c("seqnames", "seqname", "seq_names", "chr")
            seqname_field <- seqname_fields[seqname_fields %in%
                names(ranges_df)][1]
            if (is.na(seqname_field) || is.null(seqname_field)) {
                return(NULL)
            }
            if (!all(c("start", "end") %in% names(ranges_df))) {
                return(NULL)
            }
            gr <- GenomicRanges::makeGRangesFromDataFrame(
                ranges_df,
                keep.extra.columns = TRUE,
                seqnames.field = seqname_field,
                start.field = "start",
                end.field = "end",
                strand.field = if ("strand" %in% names(ranges_df)) {
                    "strand"
                } else {
                    NULL
                }
            )
            if (!is.null(feature_ids)) {
                names(gr) <- as.character(feature_ids)
            }
            if (!is.null(feature_ids) &&
                length(feature_ids) == length(gr) &&
                !is.null(names(gr)) &&
                all(feature_ids %in% names(gr))) {
                gr <- gr[match(feature_ids, names(gr))]
            }
            gr
        },
        .load_metadata = function(prefix, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "metadata"), ref = ref,
                    project = project),
                error = function(e) list()
            )
        },
        .manifest_slot_mode = function(manifest, field) {
            value <- manifest[[field]]
            if (is.null(value) || !nzchar(as.character(value))) {
                return("table")
            }
            as.character(value)
        },
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
                    stop("Matrix package required for sparse matrices",
                        call. = FALSE)
                }
                # Use Matrix::summary for sparse matrices (only non-zero values)
                summ <- Matrix::summary(mat)
                data.frame(
                    feature = rownames(mat)[summ$i],
                    sample = colnames(mat)[summ$j],
                    value = summ$x,
                    feature_idx = as.integer(summ$i),
                    sample_idx = as.integer(summ$j),
                    stringsAsFactors = FALSE
                )
            } else {
                nz <- which(mat != 0, arr.ind = TRUE)
                if (nrow(nz) == 0) {
                    return(data.frame(
                        feature = character(0),
                        sample = character(0),
                        value = numeric(0),
                        feature_idx = integer(0),
                        sample_idx = integer(0),
                        stringsAsFactors = FALSE
                    ))
                }
                data.frame(
                    feature = rownames(mat)[nz[, 1]],
                    sample = colnames(mat)[nz[, 2]],
                    value = as.numeric(mat[nz]),
                    feature_idx = as.integer(nz[, 1]),
                    sample_idx = as.integer(nz[, 2]),
                    stringsAsFactors = FALSE
                )
            }
        },

        # Convert long format back to matrix
        .long_to_matrix = function(long_df, feature_ids = NULL,
            sample_ids = NULL) {
            if (is.null(feature_ids)) {
                feature_ids <- unique(long_df$feature)
            }
            if (is.null(sample_ids)) {
                sample_ids <- unique(long_df$sample)
            }
            feature_ids <- as.character(feature_ids)
            sample_ids <- as.character(sample_ids)
            if (length(feature_ids) == 0 || length(sample_ids) == 0) {
                return(matrix(0, 0, 0))
            }
            if (nrow(long_df) == 0) {
                if (requireNamespace("Matrix", quietly = TRUE)) {
                    return(Matrix::sparseMatrix(
                        i = integer(0),
                        j = integer(0),
                        x = numeric(0),
                        dims = c(length(feature_ids), length(sample_ids)),
                        dimnames = list(feature_ids, sample_ids)
                    ))
                }
                return(matrix(0, length(feature_ids), length(sample_ids),
                    dimnames = list(feature_ids, sample_ids)))
            }
            i_idx <- if ("feature_idx" %in% names(long_df)) {
                as.integer(long_df$feature_idx)
            } else {
                match(long_df$feature, feature_ids)
            }
            j_idx <- if ("sample_idx" %in% names(long_df)) {
                as.integer(long_df$sample_idx)
            } else {
                match(long_df$sample, sample_ids)
            }

            # Create sparse matrix
            if (requireNamespace("Matrix", quietly = TRUE)) {
                mat <- Matrix::sparseMatrix(
                    i = i_idx,
                    j = j_idx,
                    x = long_df$value,
                    dims = c(length(feature_ids), length(sample_ids)),
                    dimnames = list(feature_ids, sample_ids)
                )
            } else {
                # Fall back to dense matrix
                mat <- matrix(0, length(feature_ids), length(sample_ids),
                    dimnames = list(feature_ids, sample_ids)
                )
                for (k in seq_len(nrow(long_df))) {
                    mat[i_idx[k], j_idx[k]] <- long_df$value[k]
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
