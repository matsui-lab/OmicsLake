#' @title MsExperiment Adapter
#' @description Adapter for storing and retrieving
#' \code{MsExperiment} objects.
#'
#' @details
#' This adapter preserves \code{MsExperiment} components:
#' \itemize{
#'   \item sample metadata (\code{sampleData})
#'   \item spectra (\code{spectra})
#'   \item quantitative/assay data (\code{qdata})
#'   \item experiment files (\code{experimentFiles})
#'   \item auxiliary data (\code{otherData})
#' }
#'
#' @importFrom R6 R6Class
#' @export
MsExperimentAdapter <- R6::R6Class("MsExperimentAdapter",
    inherit = LakeAdapter,
    public = list(
        name = function() {
            "MsExperiment"
        },
        can_handle = function(data) {
            inherits(data, "MsExperiment")
        },
        priority = function() {
            125
        },
        put = function(lake, name, data) {
            if (!requireNamespace("MsExperiment", quietly = TRUE)) {
                stop("MsExperiment package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__mse__.")
            project <- lake$.__enclos_env__$private$.project

            sample_data_mode <- private$.put_sample_data(prefix, data, project)
            spectra_info <- private$.put_component_with_adapter(
                prefix = prefix,
                component_name = "spectra",
                value = private$.extract_spectra(data),
                lake = lake,
                project = project
            )
            qdata_info <- private$.put_component_with_adapter(
                prefix = prefix,
                component_name = "qdata",
                value = private$.extract_qdata(data),
                lake = lake,
                project = project
            )
            has_experiment_files <- private$.put_experiment_files(prefix, data,
                project)
            has_other_data <- private$.put_other_data(prefix, data, project)
            has_metadata <- private$.put_metadata(prefix, data, project)
            has_sample_links <- private$.put_sample_links(prefix, data, project)

            private$.put_manifest(prefix, data, sample_data_mode, spectra_info,
                qdata_info, has_experiment_files, has_other_data, has_metadata,
                has_sample_links, project)
            invisible(TRUE)
        },
        get = function(lake, name, ref = "@latest") {
            if (!requireNamespace("MsExperiment", quietly = TRUE)) {
                stop("MsExperiment package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__mse__.")
            project <- lake$.__enclos_env__$private$.project
            manifest <- private$.load_manifest(prefix, name, ref, project)

            sample_data <- private$.load_sample_data(prefix, manifest, ref,
                project)
            spectra <- private$.load_component_with_adapter(
                prefix = prefix,
                component_name = "spectra",
                mode = private$.manifest_scalar(manifest$spectra_mode, "none"),
                adapter_name = private$.manifest_scalar(
                    manifest$spectra_adapter,
                    NA_character_
                ),
                lake = lake,
                ref = ref,
                project = project
            )
            qdata <- private$.load_component_with_adapter(
                prefix = prefix,
                component_name = "qdata",
                mode = private$.manifest_scalar(manifest$qdata_mode, "none"),
                adapter_name = private$.manifest_scalar(manifest$qdata_adapter,
                    NA_character_),
                lake = lake,
                ref = ref,
                project = project
            )
            experiment_files <- private$.load_experiment_files(prefix, manifest,
                ref, project)
            other_data <- private$.load_other_data(prefix, manifest, ref,
                project)

            mse <- MsExperiment::MsExperiment(
                experimentFiles = experiment_files,
                otherData = other_data,
                qdata = qdata,
                sampleData = S4Vectors::DataFrame(sample_data),
                spectra = spectra
            )
            if (isTRUE(manifest$has_metadata)) {
                S4Vectors::metadata(mse) <- private$.load_metadata(prefix, ref,
                    project)
            }
            if (isTRUE(manifest$has_sample_links)) {
                links <- private$.load_sample_links(prefix, ref, project)
                if (!is.null(links)) {
                    try({
                        methods::slot(mse, "sampleDataLinks") <- links
                        methods::validObject(mse)
                    }, silent = TRUE)
                }
            }
            mse
        },
        components = function(lake, name) {
            prefix <- paste0(name, ".__mse__.")
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
            prefix <- paste0(name, ".__mse__.")
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
            manifest_pattern <- "\\.__mse__\\.manifest$"
            manifests <- objects$name[grepl(manifest_pattern, objects$name)]
            if (!length(manifests)) {
                return(character(0))
            }
            roots <- sort(unique(sub(manifest_pattern, "", manifests)))
            roots[!grepl(
                "\\.__(se|sce|mae|spectra|qfeatures|mse|xcms|chrom|seurat)__\\.",
                roots,
                perl = TRUE
            )]
        }
    ),
    private = list(
        .extract_spectra = function(data) {
            tryCatch(
                MsExperiment::spectra(data),
                error = function(e) NULL
            )
        },
        .extract_qdata = function(data) {
            tryCatch(
                MsExperiment::qdata(data),
                error = function(e) NULL
            )
        },
        .put_sample_data = function(prefix, data, project) {
            sample_data <- as.data.frame(MsExperiment::sampleData(data))
            sample_ids <- rownames(sample_data)
            if (is.null(sample_ids) || length(sample_ids) != nrow(sample_data) ||
                anyNA(sample_ids) || any(!nzchar(as.character(sample_ids)))) {
                sample_ids <- paste0("sample", seq_len(nrow(sample_data)))
            }
            sample_data$.__sample_id__ <- as.character(sample_ids)
            has_list_like <- any(vapply(sample_data, function(x) {
                is.list(x) || inherits(x, "List")
            }, logical(1)))
            if (has_list_like) {
                ol_save(paste0(prefix, "sampleData.object"), sample_data,
                    project = project)
                return("object")
            }
            ol_write(paste0(prefix, "sampleData"), sample_data,
                project = project, mode = "overwrite")
            "table"
        },
        .put_component_with_adapter = function(prefix, component_name, value,
            lake, project) {
            if (is.null(value)) {
                return(list(mode = "none", adapter = NA_character_))
            }
            has_content <- tryCatch(length(value) > 0L, error = function(e) TRUE)
            if (!has_content) {
                return(list(mode = "none", adapter = NA_character_))
            }
            ref_name <- paste0(prefix, component_name)
            adapter <- find_adapter(value)
            if (!is.null(adapter)) {
                adapter$put(lake, ref_name, value)
                return(list(mode = "adapter", adapter = adapter$name()))
            }
            ol_save(ref_name, value, project = project)
            list(mode = "object", adapter = NA_character_)
        },
        .load_component_with_adapter = function(prefix, component_name, mode,
            adapter_name, lake, ref, project) {
            mode <- as.character(mode)
            ref_name <- paste0(prefix, component_name)
            if (identical(mode, "none")) {
                return(NULL)
            }
            if (identical(mode, "adapter")) {
                obj <- private$.load_via_adapter(ref_name, adapter_name, lake,
                    ref)
                if (!is.null(obj)) {
                    return(obj)
                }
            }
            tryCatch(
                ol_read_object(ref_name, ref = ref, project = project),
                error = function(e) NULL
            )
        },
        .load_via_adapter = function(name, adapter_name, lake, ref) {
            adapters <- get_adapters()
            if (!length(adapters)) {
                return(NULL)
            }
            preferred <- as.character(adapter_name)
            ordered <- adapters
            if (!is.na(preferred) && nzchar(preferred) &&
                preferred %in% names(adapters)) {
                ordered <- c(list(adapters[[preferred]]),
                    adapters[setdiff(names(adapters), preferred)])
            }
            for (adapter in ordered) {
                obj <- tryCatch(
                    adapter$get(lake, name, ref = ref),
                    error = function(e) NULL
                )
                if (!is.null(obj)) {
                    return(obj)
                }
            }
            NULL
        },
        .put_experiment_files = function(prefix, data, project) {
            ef <- tryCatch(
                MsExperiment::experimentFiles(data),
                error = function(e) NULL
            )
            if (is.null(ef) || length(ef) == 0L) {
                return(FALSE)
            }
            ol_save(paste0(prefix, "experimentFiles"), ef, project = project)
            TRUE
        },
        .put_other_data = function(prefix, data, project) {
            od <- tryCatch(MsExperiment::otherData(data), error = function(e) NULL)
            if (is.null(od) || length(od) == 0L) {
                return(FALSE)
            }
            ol_save(paste0(prefix, "otherData"), od, project = project)
            TRUE
        },
        .put_metadata = function(prefix, data, project) {
            meta <- tryCatch(S4Vectors::metadata(data), error = function(e) list())
            if (!length(meta)) {
                return(FALSE)
            }
            ol_save(paste0(prefix, "metadata"), meta, project = project)
            TRUE
        },
        .put_sample_links = function(prefix, data, project) {
            links <- tryCatch(methods::slot(data, "sampleDataLinks"),
                error = function(e) NULL)
            if (is.null(links) || length(links) == 0L) {
                return(FALSE)
            }
            ol_save(paste0(prefix, "sampleDataLinks"), links, project = project)
            TRUE
        },
        .put_manifest = function(prefix, data, sample_data_mode, spectra_info,
            qdata_info, has_experiment_files, has_other_data, has_metadata,
            has_sample_links, project) {
            manifest <- list(
                type = "MsExperiment",
                class = class(data)[1],
                sample_data_mode = as.character(sample_data_mode),
                spectra_mode = as.character(spectra_info$mode),
                spectra_adapter = as.character(spectra_info$adapter),
                qdata_mode = as.character(qdata_info$mode),
                qdata_adapter = as.character(qdata_info$adapter),
                has_experiment_files = isTRUE(has_experiment_files),
                has_other_data = isTRUE(has_other_data),
                has_metadata = isTRUE(has_metadata),
                has_sample_links = isTRUE(has_sample_links),
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
                    stop("Cannot find MsExperiment manifest for '", name, "'",
                        call. = FALSE)
                }
            )
        },
        .load_sample_data = function(prefix, manifest, ref, project) {
            mode <- private$.manifest_scalar(manifest$sample_data_mode, "table")
            sample_data <- if (identical(mode, "object")) {
                tryCatch(
                    ol_read_object(paste0(prefix, "sampleData.object"), ref = ref,
                        project = project),
                    error = function(e) NULL
                )
            } else {
                tryCatch(
                    ol_read(paste0(prefix, "sampleData"), ref = ref,
                        project = project),
                    error = function(e) NULL
                )
            }
            if (is.null(sample_data)) {
                sample_data <- data.frame(.__sample_id__ = character(0),
                    stringsAsFactors = FALSE)
            }
            ids <- if (".__sample_id__" %in% names(sample_data)) {
                as.character(sample_data$.__sample_id__)
            } else {
                paste0("sample", seq_len(nrow(sample_data)))
            }
            sample_data$.__sample_id__ <- NULL
            rownames(sample_data) <- ids
            sample_data
        },
        .load_experiment_files = function(prefix, manifest, ref, project) {
            if (!isTRUE(manifest$has_experiment_files)) {
                return(MsExperiment::MsExperimentFiles())
            }
            ef <- tryCatch(
                ol_read_object(paste0(prefix, "experimentFiles"), ref = ref,
                    project = project),
                error = function(e) NULL
            )
            if (!is.null(ef)) ef else MsExperiment::MsExperimentFiles()
        },
        .load_other_data = function(prefix, manifest, ref, project) {
            if (!isTRUE(manifest$has_other_data)) {
                return(S4Vectors::List())
            }
            od <- tryCatch(
                ol_read_object(paste0(prefix, "otherData"), ref = ref,
                    project = project),
                error = function(e) NULL
            )
            if (!is.null(od)) od else S4Vectors::List()
        },
        .load_metadata = function(prefix, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "metadata"), ref = ref,
                    project = project),
                error = function(e) list()
            )
        },
        .load_sample_links = function(prefix, ref, project) {
            tryCatch(
                ol_read_object(paste0(prefix, "sampleDataLinks"), ref = ref,
                    project = project),
                error = function(e) NULL
            )
        },
        .manifest_scalar = function(x, default = "") {
            vals <- as.character(x)
            if (!length(vals) || is.na(vals[[1]])) {
                return(default)
            }
            vals[[1]]
        }
    )
)
