#' @title SingleCellExperiment Adapter
#' @description Adapter for storing and retrieving
#' SingleCellExperiment objects.
#'
#' @details
#' This adapter extends SummarizedExperiment support by preserving
#' SingleCellExperiment-specific components:
#' \itemize{
#'   \item reduced dimensions (\code{reducedDims})
#'   \item alternative experiments (\code{altExps})
#'   \item row/column pairings (\code{rowPairs}, \code{colPairs})
#'   \item size factors (\code{sizeFactors})
#'   \item column labels (\code{colLabels})
#'   \item main experiment label (\code{mainExpName})
#' }
#'
#' @importFrom R6 R6Class
#' @export
SCEAdapter <- R6::R6Class("SCEAdapter",
    inherit = LakeAdapter,
    public = list(
        name = function() {
            "SingleCellExperiment"
        },
        can_handle = function(data) {
            inherits(data, "SingleCellExperiment")
        },
        priority = function() {
            110
        },
        put = function(lake, name, data) {
            if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
                stop("SingleCellExperiment package is required",
                    call. = FALSE)
            }
            prefix <- paste0(name, ".__sce__.")
            project <- lake$.__enclos_env__$private$.project
            private$.put_main_experiment(prefix, data, lake)
            reduced_dim_names <- private$.put_reduced_dims(prefix, data,
                project)
            alt_exp_info <- private$.put_alt_exps(prefix, data, lake)
            row_pair_names <- private$.put_row_pairs(prefix, data, project)
            col_pair_names <- private$.put_col_pairs(prefix, data, project)
            has_size_factors <- private$.put_size_factors(prefix, data, project)
            has_col_labels <- private$.put_col_labels(prefix, data, project)
            main_exp_name <- private$.put_main_exp_name(data)
            internal_extra_flags <- private$.put_internal_extras(prefix, data,
                project)
            private$.put_manifest(prefix, data, reduced_dim_names,
                alt_exp_info$names, alt_exp_info$types, row_pair_names,
                col_pair_names, has_size_factors, has_col_labels,
                main_exp_name, internal_extra_flags, project)
            invisible(TRUE)
        },
        get = function(lake, name, ref = "@latest") {
            if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
                stop("SingleCellExperiment package is required",
                    call. = FALSE)
            }
            prefix <- paste0(name, ".__sce__.")
            project <- lake$.__enclos_env__$private$.project
            manifest <- private$.load_manifest(prefix, name, ref, project)
            se_adapter <- SEAdapter$new()
            main_se <- se_adapter$get(lake, paste0(prefix, "main"), ref = ref)
            sce <- methods::as(main_se, "SingleCellExperiment")

            if (length(manifest$reduced_dim_names) > 0) {
                for (rd_name in manifest$reduced_dim_names) {
                    rd <- ol_read_object(paste0(prefix, "reducedDim.", rd_name),
                        ref = ref, project = project)
                    SingleCellExperiment::reducedDim(sce, rd_name) <- rd
                }
            }

            if (length(manifest$alt_exp_names) > 0) {
                for (alt_name in manifest$alt_exp_names) {
                    alt_ref <- paste0(prefix, "altExp.", alt_name)
                    alt_type <- private$.manifest_alt_type(manifest, alt_name)
                    if (identical(alt_type, "sce")) {
                        alt_obj <- SCEAdapter$new()$get(lake, alt_ref, ref = ref)
                    } else {
                        alt_obj <- se_adapter$get(lake, alt_ref, ref = ref)
                    }
                    SingleCellExperiment::altExp(sce, alt_name) <- alt_obj
                }
            }
            sce <- private$.restore_row_pairs(prefix, sce, manifest, ref,
                project)
            sce <- private$.restore_col_pairs(prefix, sce, manifest, ref,
                project)

            if (isTRUE(manifest$has_size_factors)) {
                sf <- ol_read_object(paste0(prefix, "sizeFactors"), ref = ref,
                    project = project)
                SingleCellExperiment::sizeFactors(sce) <- sf
            }
            if (isTRUE(manifest$has_col_labels)) {
                labels <- ol_read_object(paste0(prefix, "colLabels"),
                    ref = ref, project = project)
                SingleCellExperiment::colLabels(sce) <- labels
            }
            if (isTRUE(manifest$has_main_exp_name)) {
                SingleCellExperiment::mainExpName(sce) <- manifest$main_exp_name
            }
            sce <- private$.restore_internal_extras(prefix, sce, manifest, ref,
                project)

            sce
        },
        components = function(lake, name) {
            prefix <- paste0(name, ".__sce__.")
            prefix_pattern <- paste0("^", gsub(
                "([.|()\\^{}+$*?\\[\\]\\\\])",
                "\\\\\\1",
                prefix,
                perl = TRUE
            ))
            tables <- lake$tables()
            objects <- lake$objects()
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
            components$component_id <- gsub(prefix_pattern, "",
                components$component)
            components
        },
        exists = function(lake, name) {
            prefix <- paste0(name, ".__sce__.")
            objects <- tryCatch(
                lake$objects(),
                error = function(e) data.frame(name = character(0))
            )
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
            manifest_pattern <- "\\.__sce__\\.manifest$"
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
        .put_main_experiment = function(prefix, data, lake) {
            main_name <- paste0(prefix, "main")
            se_data <- methods::as(data, "RangedSummarizedExperiment")
            # Preserve original row/column identifiers from the source SCE.
            if (!is.null(rownames(data))) {
                rownames(se_data) <- rownames(data)
            }
            if (!is.null(colnames(data))) {
                colnames(se_data) <- colnames(data)
            }
            se_adapter <- SEAdapter$new()
            se_adapter$put(lake, main_name, se_data)
            invisible(TRUE)
        },
        .put_reduced_dims = function(prefix, data, project) {
            rdims <- SingleCellExperiment::reducedDims(data)
            rd_names <- names(rdims)
            if (is.null(rd_names) && length(rdims) > 0) {
                rd_names <- paste0("rd", seq_along(rdims))
            }
            for (i in seq_along(rd_names)) {
                ol_save(paste0(prefix, "reducedDim.", rd_names[[i]]), rdims[[i]],
                    project = project)
            }
            if (is.null(rd_names)) character(0) else rd_names
        },
        .put_alt_exps = function(prefix, data, lake) {
            alt_names <- SingleCellExperiment::altExpNames(data)
            if (length(alt_names) == 0) {
                return(list(names = character(0), types = character(0)))
            }
            alt_types <- stats::setNames(character(length(alt_names)), alt_names)
            for (alt_name in alt_names) {
                alt_obj <- SingleCellExperiment::altExp(data, alt_name)
                alt_ref <- paste0(prefix, "altExp.", alt_name)
                adapter <- find_adapter(alt_obj)
                if (!is.null(adapter)) {
                    adapter$put(lake, alt_ref, alt_obj)
                } else {
                    SEAdapter$new()$put(lake, alt_ref, alt_obj)
                }
                alt_types[[alt_name]] <- if (inherits(alt_obj,
                    "SingleCellExperiment")) "sce" else "se"
            }
            list(names = alt_names, types = alt_types)
        },
        .put_row_pairs = function(prefix, data, project) {
            pair_names <- tryCatch(
                SingleCellExperiment::rowPairNames(data),
                error = function(e) character(0)
            )
            if (!length(pair_names)) {
                return(character(0))
            }
            for (pair_name in pair_names) {
                pair_obj <- SingleCellExperiment::rowPair(data, pair_name)
                ol_save(paste0(prefix, "rowPair.", pair_name), pair_obj,
                    project = project)
            }
            pair_names
        },
        .put_col_pairs = function(prefix, data, project) {
            pair_names <- tryCatch(
                SingleCellExperiment::colPairNames(data),
                error = function(e) character(0)
            )
            if (!length(pair_names)) {
                return(character(0))
            }
            for (pair_name in pair_names) {
                pair_obj <- SingleCellExperiment::colPair(data, pair_name)
                ol_save(paste0(prefix, "colPair.", pair_name), pair_obj,
                    project = project)
            }
            pair_names
        },
        .put_size_factors = function(prefix, data, project) {
            sf <- tryCatch(
                SingleCellExperiment::sizeFactors(data),
                error = function(e) NULL
            )
            if (is.null(sf)) {
                return(FALSE)
            }
            ol_save(paste0(prefix, "sizeFactors"), sf, project = project)
            TRUE
        },
        .put_col_labels = function(prefix, data, project) {
            labels <- tryCatch(
                SingleCellExperiment::colLabels(data),
                error = function(e) NULL
            )
            if (is.null(labels)) {
                return(FALSE)
            }
            ol_save(paste0(prefix, "colLabels"), labels, project = project)
            TRUE
        },
        .put_main_exp_name = function(data) {
            main_exp_name <- tryCatch(
                SingleCellExperiment::mainExpName(data),
                error = function(e) NULL
            )
            if (is.null(main_exp_name) || length(main_exp_name) == 0) {
                return(NULL)
            }
            main_exp_name <- as.character(main_exp_name[[1]])
            if (is.na(main_exp_name) || !nzchar(main_exp_name)) {
                return(NULL)
            }
            main_exp_name
        },
        .put_internal_extras = function(prefix, data, project) {
            # Preserve only user-level extras; core SCE internals are rebuilt
            # explicitly by this adapter.
            int_md <- tryCatch(
                SingleCellExperiment::int_metadata(data),
                error = function(e) NULL
            )
            extra_md <- list()
            if (!is.null(int_md) && length(int_md) > 0) {
                keep_md <- setdiff(names(int_md), c("version", "mainExpName"))
                if (length(keep_md) > 0) {
                    extra_md <- int_md[keep_md]
                }
            }
            if (length(extra_md) > 0) {
                ol_save(paste0(prefix, "int_metadata.extra"), extra_md,
                    project = project)
            }

            int_cd <- tryCatch(
                SingleCellExperiment::int_colData(data),
                error = function(e) NULL
            )
            has_int_cd_extra <- FALSE
            if (!is.null(int_cd) && nrow(int_cd) > 0) {
                keep_cd <- setdiff(colnames(int_cd), c("reducedDims",
                    "altExps", "colPairs"))
                if (length(keep_cd) > 0) {
                    extra_cd <- int_cd[, keep_cd, drop = FALSE]
                    ol_save(paste0(prefix, "int_colData.extra"), extra_cd,
                        project = project)
                    has_int_cd_extra <- TRUE
                }
            }

            int_ed <- tryCatch(
                SingleCellExperiment::int_elementMetadata(data),
                error = function(e) NULL
            )
            has_int_ed_extra <- FALSE
            if (!is.null(int_ed) && nrow(int_ed) > 0) {
                keep_ed <- setdiff(colnames(int_ed), "rowPairs")
                if (length(keep_ed) > 0) {
                    extra_ed <- int_ed[, keep_ed, drop = FALSE]
                    ol_save(paste0(prefix, "int_elementMetadata.extra"),
                        extra_ed, project = project)
                    has_int_ed_extra <- TRUE
                }
            }

            list(
                has_int_metadata_extra = length(extra_md) > 0,
                has_int_colData_extra = isTRUE(has_int_cd_extra),
                has_int_elementMetadata_extra = isTRUE(has_int_ed_extra)
            )
        },
        .put_manifest = function(prefix, data, reduced_dim_names, alt_exp_names,
            alt_exp_types, row_pair_names, col_pair_names, has_size_factors,
            has_col_labels, main_exp_name, internal_extra_flags, project) {
            manifest <- list(
                type = "SingleCellExperiment",
                class = class(data)[1],
                reduced_dim_names = reduced_dim_names,
                alt_exp_names = alt_exp_names,
                alt_exp_types = alt_exp_types,
                row_pair_names = row_pair_names,
                col_pair_names = col_pair_names,
                has_size_factors = isTRUE(has_size_factors),
                has_col_labels = isTRUE(has_col_labels),
                has_main_exp_name = !is.null(main_exp_name),
                main_exp_name = if (!is.null(main_exp_name)) {
                    main_exp_name
                } else {
                    NA_character_
                },
                has_int_metadata_extra = isTRUE(
                    internal_extra_flags$has_int_metadata_extra
                ),
                has_int_colData_extra = isTRUE(
                    internal_extra_flags$has_int_colData_extra
                ),
                has_int_elementMetadata_extra = isTRUE(
                    internal_extra_flags$has_int_elementMetadata_extra
                ),
                created_at = Sys.time()
            )
            ol_save(paste0(prefix, "manifest"), manifest, project = project)
            invisible(TRUE)
        },
        .manifest_alt_type = function(manifest, alt_name) {
            if (is.null(manifest$alt_exp_types) ||
                !length(manifest$alt_exp_types)) {
                return("se")
            }
            alt_types <- manifest$alt_exp_types
            if (!is.null(names(alt_types)) && alt_name %in% names(alt_types)) {
                return(as.character(alt_types[[alt_name]]))
            }
            if (length(alt_types) == length(manifest$alt_exp_names)) {
                idx <- which(manifest$alt_exp_names == alt_name)
                if (length(idx) == 1) {
                    return(as.character(alt_types[[idx]]))
                }
            }
            "se"
        },
        .restore_row_pairs = function(prefix, sce, manifest, ref, project) {
            pair_names <- manifest$row_pair_names
            if (is.null(pair_names) || !length(pair_names)) {
                return(sce)
            }
            for (pair_name in pair_names) {
                pair_obj <- ol_read_object(paste0(prefix, "rowPair.", pair_name),
                    ref = ref, project = project)
                SingleCellExperiment::rowPair(sce, pair_name) <- pair_obj
            }
            sce
        },
        .restore_col_pairs = function(prefix, sce, manifest, ref, project) {
            pair_names <- manifest$col_pair_names
            if (is.null(pair_names) || !length(pair_names)) {
                return(sce)
            }
            for (pair_name in pair_names) {
                pair_obj <- ol_read_object(paste0(prefix, "colPair.", pair_name),
                    ref = ref, project = project)
                SingleCellExperiment::colPair(sce, pair_name) <- pair_obj
            }
            sce
        },
        .restore_internal_extras = function(prefix, sce, manifest, ref, project) {
            if (isTRUE(manifest$has_int_metadata_extra)) {
                extra_md <- tryCatch(
                    ol_read_object(paste0(prefix, "int_metadata.extra"),
                        ref = ref, project = project),
                    error = function(e) NULL
                )
                if (!is.null(extra_md) && length(extra_md) > 0) {
                    current_md <- tryCatch(
                        SingleCellExperiment::int_metadata(sce),
                        error = function(e) list()
                    )
                    current_md[names(extra_md)] <- extra_md
                    SingleCellExperiment::int_metadata(sce) <- current_md
                }
            }
            if (isTRUE(manifest$has_int_colData_extra)) {
                extra_cd <- tryCatch(
                    ol_read_object(paste0(prefix, "int_colData.extra"),
                        ref = ref, project = project),
                    error = function(e) NULL
                )
                if (!is.null(extra_cd)) {
                    extra_cd <- S4Vectors::DataFrame(extra_cd)
                    if (nrow(extra_cd) == ncol(sce)) {
                        for (nm in colnames(extra_cd)) {
                            SingleCellExperiment::int_colData(sce)[[nm]] <-
                                extra_cd[[nm]]
                        }
                    }
                }
            }
            if (isTRUE(manifest$has_int_elementMetadata_extra)) {
                extra_ed <- tryCatch(
                    ol_read_object(paste0(prefix, "int_elementMetadata.extra"),
                        ref = ref, project = project),
                    error = function(e) NULL
                )
                if (!is.null(extra_ed)) {
                    extra_ed <- S4Vectors::DataFrame(extra_ed)
                    if (nrow(extra_ed) == nrow(sce)) {
                        for (nm in colnames(extra_ed)) {
                            SingleCellExperiment::int_elementMetadata(
                                sce
                            )[[nm]] <- extra_ed[[nm]]
                        }
                    }
                }
            }
            sce
        },
        .load_manifest = function(prefix, name, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "manifest"), ref = ref,
                    project = project),
                error = function(e) {
                    stop(
                        "Cannot find SingleCellExperiment manifest for '",
                        name,
                        "'",
                        call. = FALSE
                    )
                }
            )
        }
    )
)
