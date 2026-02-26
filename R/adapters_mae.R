#' @title MultiAssayExperiment Adapter
#' @description Adapter for storing and retrieving
#' \code{MultiAssayExperiment} objects.
#'
#' @details
#' This adapter preserves \code{MultiAssayExperiment} components:
#' \itemize{
#'   \item experiments (\code{experiments})
#'   \item sample map (\code{sampleMap})
#'   \item participant metadata (\code{colData})
#'   \item dropped samples (\code{drops})
#'   \item object metadata (\code{metadata})
#' }
#'
#' @importFrom R6 R6Class
#' @export
MAEAdapter <- R6::R6Class("MAEAdapter",
    inherit = LakeAdapter,
    public = list(
        name = function() {
            "MultiAssayExperiment"
        },
        can_handle = function(data) {
            inherits(data, "MultiAssayExperiment")
        },
        priority = function() {
            120
        },
        put = function(lake, name, data) {
            if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
                stop("MultiAssayExperiment package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__mae__.")
            project <- lake$.__enclos_env__$private$.project

            exp_info <- private$.put_experiments(prefix, data, lake, project)
            col_data_mode <- private$.put_col_data(prefix, data, project)
            private$.put_sample_map(prefix, data, project)
            has_drops <- private$.put_drops(prefix, data, project)
            has_meta <- private$.put_metadata(prefix, data, project)
            private$.put_manifest(prefix, data, exp_info, col_data_mode,
                has_drops, has_meta, project)
            invisible(TRUE)
        },
        get = function(lake, name, ref = "@latest") {
            if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
                stop("MultiAssayExperiment package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__mae__.")
            project <- lake$.__enclos_env__$private$.project
            manifest <- private$.load_manifest(prefix, name, ref, project)

            exp_list <- private$.load_experiments(prefix, manifest, lake, ref,
                project)
            col_data <- private$.load_col_data(prefix, manifest, ref, project)
            sample_map <- private$.load_sample_map(prefix, ref, project)
            if (is.null(sample_map)) {
                sample_map <- S4Vectors::DataFrame(
                    assay = character(0),
                    primary = character(0),
                    colname = character(0)
                )
            }

            mae <- MultiAssayExperiment::MultiAssayExperiment(
                experiments = exp_list,
                colData = S4Vectors::DataFrame(col_data),
                sampleMap = S4Vectors::DataFrame(sample_map)
            )
            if (isTRUE(manifest$has_drops)) {
                MultiAssayExperiment::drops(mae) <- private$.load_drops(prefix,
                    ref, project)
            }
            if (isTRUE(manifest$has_metadata)) {
                S4Vectors::metadata(mae) <- private$.load_metadata(prefix, ref,
                    project)
            }
            mae
        },
        components = function(lake, name) {
            prefix <- paste0(name, ".__mae__.")
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
            prefix <- paste0(name, ".__mae__.")
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
            manifest_pattern <- "\\.__mae__\\.manifest$"
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
        .put_experiments = function(prefix, data, lake, project) {
            exps <- MultiAssayExperiment::experiments(data)
            exp_names <- names(exps)
            if (is.null(exp_names) && length(exps) > 0) {
                exp_names <- paste0("experiment", seq_along(exps))
            }
            exp_keys <- paste0("exp", seq_along(exps))
            exp_modes <- character(length(exps))
            exp_adapters <- character(length(exps))

            for (i in seq_along(exps)) {
                exp_obj <- exps[[i]]
                exp_ref <- paste0(prefix, "experiment.", exp_keys[[i]])
                adapter <- find_adapter(exp_obj)
                if (!is.null(adapter)) {
                    adapter$put(lake, exp_ref, exp_obj)
                    exp_modes[[i]] <- "adapter"
                    exp_adapters[[i]] <- adapter$name()
                } else {
                    ol_save(exp_ref, exp_obj, project = project)
                    exp_modes[[i]] <- "object"
                    exp_adapters[[i]] <- NA_character_
                }
            }

            list(
                names = if (is.null(exp_names)) character(0) else exp_names,
                keys = exp_keys,
                modes = exp_modes,
                adapters = exp_adapters
            )
        },
        .put_col_data = function(prefix, data, project) {
            col_data <- as.data.frame(SummarizedExperiment::colData(data))
            primary_ids <- rownames(SummarizedExperiment::colData(data))
            if (is.null(primary_ids) || length(primary_ids) != nrow(col_data)) {
                primary_ids <- rownames(col_data)
            }
            if (is.null(primary_ids) || length(primary_ids) != nrow(col_data) ||
                anyNA(primary_ids) || any(!nzchar(as.character(primary_ids)))) {
                primary_ids <- paste0("primary", seq_len(nrow(col_data)))
            }

            if (nrow(col_data) == 0) {
                col_data <- data.frame(
                    .__primary_id__ = character(0),
                    stringsAsFactors = FALSE
                )
            } else {
                col_data$.__primary_id__ <- as.character(primary_ids)
            }

            if (any(vapply(col_data, is.list, logical(1)))) {
                ol_save(paste0(prefix, "colData.object"), col_data,
                    project = project)
                return("object")
            }
            ol_write(paste0(prefix, "colData"), col_data, project = project,
                mode = "overwrite")
            "table"
        },
        .put_sample_map = function(prefix, data, project) {
            sm <- MultiAssayExperiment::sampleMap(data)
            ol_save(paste0(prefix, "sampleMap"), sm, project = project)
            invisible(TRUE)
        },
        .put_drops = function(prefix, data, project) {
            dr <- tryCatch(
                MultiAssayExperiment::drops(data),
                error = function(e) list()
            )
            if (length(dr) > 0) {
                ol_save(paste0(prefix, "drops"), dr, project = project)
                return(TRUE)
            }
            FALSE
        },
        .put_metadata = function(prefix, data, project) {
            meta <- S4Vectors::metadata(data)
            if (length(meta) > 0) {
                ol_save(paste0(prefix, "metadata"), meta, project = project)
                return(TRUE)
            }
            FALSE
        },
        .put_manifest = function(prefix, data, exp_info, col_data_mode,
            has_drops, has_meta, project) {
            primary_ids <- rownames(SummarizedExperiment::colData(data))
            if (is.null(primary_ids)) {
                primary_ids <- character(0)
            }
            manifest <- list(
                type = "MultiAssayExperiment",
                class = class(data)[1],
                experiment_names = exp_info$names,
                experiment_keys = exp_info$keys,
                experiment_modes = exp_info$modes,
                experiment_adapters = exp_info$adapters,
                n_experiments = length(exp_info$names),
                col_data_mode = as.character(col_data_mode),
                primary_ids = as.character(primary_ids),
                has_sample_map = TRUE,
                has_drops = isTRUE(has_drops),
                has_metadata = isTRUE(has_meta),
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
                        "Cannot find MultiAssayExperiment manifest for '",
                        name,
                        "'",
                        call. = FALSE
                    )
                }
            )
        },
        .load_experiments = function(prefix, manifest, lake, ref, project) {
            exp_names <- private$.manifest_char_vec(manifest$experiment_names)
            exp_keys <- private$.manifest_experiment_keys(manifest,
                length(exp_names))
            exp_modes <- private$.manifest_vector(manifest$experiment_modes,
                length(exp_names), default = "object")
            exp_adapters <- private$.manifest_vector(
                manifest$experiment_adapters,
                length(exp_names),
                default = NA_character_
            )

            exp_list <- vector("list", length(exp_names))
            if (length(exp_names) > 0) {
                names(exp_list) <- exp_names
            }

            for (i in seq_along(exp_names)) {
                exp_ref <- paste0(prefix, "experiment.", exp_keys[[i]])
                mode_i <- as.character(exp_modes[[i]])
                if (identical(mode_i, "adapter")) {
                    obj <- private$.load_experiment_via_adapter(
                        exp_ref = exp_ref,
                        ref = ref,
                        lake = lake,
                        preferred = exp_adapters[[i]]
                    )
                    if (is.null(obj)) {
                        obj <- tryCatch(
                            ol_read_object(exp_ref, ref = ref,
                                project = project),
                            error = function(e) NULL
                        )
                    }
                } else {
                    obj <- tryCatch(
                        ol_read_object(exp_ref, ref = ref, project = project),
                        error = function(e) NULL
                    )
                }
                if (is.null(obj)) {
                    stop("Failed to restore experiment: ", exp_names[[i]],
                        call. = FALSE)
                }
                exp_list[[i]] <- obj
            }
            exp_list
        },
        .load_experiment_via_adapter = function(exp_ref, ref, lake, preferred) {
            adapters <- get_adapters()
            if (!length(adapters)) {
                return(NULL)
            }

            ordered <- adapters
            preferred <- as.character(preferred)
            if (!is.na(preferred) && nzchar(preferred) &&
                preferred %in% names(adapters)) {
                ordered <- c(list(adapters[[preferred]]),
                    adapters[setdiff(names(adapters), preferred)])
            }

            for (adapter in ordered) {
                obj <- tryCatch(
                    adapter$get(lake, exp_ref, ref = ref),
                    error = function(e) NULL
                )
                if (!is.null(obj)) {
                    return(obj)
                }
            }
            NULL
        },
        .load_col_data = function(prefix, manifest, ref, project) {
            mode <- private$.manifest_scalar(manifest$col_data_mode, "table")
            col_data <- if (identical(mode, "object")) {
                tryCatch(
                    ol_read_object(paste0(prefix, "colData.object"), ref = ref,
                        project = project),
                    error = function(e) NULL
                )
            } else {
                tryCatch(
                    ol_read(paste0(prefix, "colData"), ref = ref,
                        project = project),
                    error = function(e) NULL
                )
            }
            if (is.null(col_data)) {
                col_data <- data.frame(
                    .__primary_id__ = private$.manifest_char_vec(
                        manifest$primary_ids
                    ),
                    stringsAsFactors = FALSE
                )
            }
            ids <- if (".__primary_id__" %in% names(col_data)) {
                as.character(col_data$.__primary_id__)
            } else {
                private$.manifest_char_vec(manifest$primary_ids)
            }
            if (".__primary_id__" %in% names(col_data)) {
                col_data$.__primary_id__ <- NULL
            }
            if (is.null(ids) || length(ids) != nrow(col_data)) {
                ids <- paste0("primary", seq_len(nrow(col_data)))
            }
            rownames(col_data) <- ids
            col_data
        },
        .load_sample_map = function(prefix, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "sampleMap"), ref = ref,
                    project = project),
                error = function(e) NULL
            )
        },
        .load_drops = function(prefix, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "drops"), ref = ref,
                    project = project),
                error = function(e) list()
            )
        },
        .load_metadata = function(prefix, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "metadata"), ref = ref,
                    project = project),
                error = function(e) list()
            )
        },
        .manifest_char_vec = function(x) {
            if (is.null(x)) {
                return(character(0))
            }
            as.character(x)
        },
        .manifest_experiment_keys = function(manifest, n) {
            keys <- private$.manifest_char_vec(manifest$experiment_keys)
            if (length(keys) == n) {
                return(keys)
            }
            paste0("exp", seq_len(n))
        },
        .manifest_vector = function(x, n, default = NA_character_) {
            vals <- private$.manifest_char_vec(x)
            if (length(vals) == n) {
                return(vals)
            }
            rep(default, n)
        },
        .manifest_scalar = function(x, default = "") {
            vals <- private$.manifest_char_vec(x)
            if (!length(vals)) {
                return(default)
            }
            vals[[1]]
        }
    )
)
