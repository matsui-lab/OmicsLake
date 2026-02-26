#' @title QFeatures Adapter
#' @description Adapter for storing and retrieving
#' \code{QFeatures} objects.
#'
#' @details
#' This adapter preserves \code{QFeatures} components:
#' \itemize{
#'   \item underlying experiments/sample maps via MultiAssayExperiment adapter
#'   \item assay links (\code{assayLinks})
#' }
#'
#' @importFrom R6 R6Class
#' @export
QFeaturesAdapter <- R6::R6Class("QFeaturesAdapter",
    inherit = LakeAdapter,
    public = list(
        name = function() {
            "QFeatures"
        },
        can_handle = function(data) {
            inherits(data, "QFeatures")
        },
        priority = function() {
            130
        },
        put = function(lake, name, data) {
            if (!requireNamespace("QFeatures", quietly = TRUE)) {
                stop("QFeatures package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__qfeatures__.")
            project <- lake$.__enclos_env__$private$.project
            mae_ref <- paste0(prefix, "mae")

            mae_adapter <- MAEAdapter$new()
            mae_data <- tryCatch(
                methods::as(data, "MultiAssayExperiment"),
                error = function(e) data
            )
            mae_adapter$put(lake, mae_ref, mae_data)

            assay_links <- private$.extract_assay_links(data)
            has_assay_links <- !is.null(assay_links)
            if (has_assay_links) {
                ol_save(paste0(prefix, "assayLinks"), assay_links,
                    project = project)
            }
            private$.put_manifest(prefix, data, has_assay_links, project)
            invisible(TRUE)
        },
        get = function(lake, name, ref = "@latest") {
            if (!requireNamespace("QFeatures", quietly = TRUE)) {
                stop("QFeatures package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__qfeatures__.")
            project <- lake$.__enclos_env__$private$.project
            manifest <- private$.load_manifest(prefix, name, ref, project)

            mae <- MAEAdapter$new()$get(lake, paste0(prefix, "mae"), ref = ref)
            assay_links <- if (isTRUE(manifest$has_assay_links)) {
                tryCatch(
                    ol_read_object(paste0(prefix, "assayLinks"), ref = ref,
                        project = project),
                    error = function(e) NULL
                )
            } else {
                NULL
            }
            private$.build_qfeatures(mae, assay_links)
        },
        components = function(lake, name) {
            prefix <- paste0(name, ".__qfeatures__.")
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
            prefix <- paste0(name, ".__qfeatures__.")
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
            manifest_pattern <- "\\.__qfeatures__\\.manifest$"
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
        .extract_assay_links = function(data) {
            tryCatch(
                QFeatures::assayLinks(data),
                error = function(e) NULL
            )
        },
        .put_manifest = function(prefix, data, has_assay_links, project) {
            manifest <- list(
                type = "QFeatures",
                class = class(data)[1],
                has_assay_links = isTRUE(has_assay_links),
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
                    stop("Cannot find QFeatures manifest for '", name, "'",
                        call. = FALSE)
                }
            )
        },
        .build_qfeatures = function(mae, assay_links = NULL) {
            exps <- MultiAssayExperiment::experiments(mae)
            col_data <- SummarizedExperiment::colData(mae)
            sample_map <- MultiAssayExperiment::sampleMap(mae)
            meta <- tryCatch(S4Vectors::metadata(mae), error = function(e) list())

            qf <- tryCatch(
                do.call(QFeatures::QFeatures, c(
                    list(
                        experiments = exps,
                        colData = col_data,
                        sampleMap = sample_map,
                        metadata = meta
                    ),
                    if (!is.null(assay_links)) list(assayLinks = assay_links)
                )),
                error = function(e) NULL
            )
            if (is.null(qf)) {
                qf <- tryCatch(QFeatures::QFeatures(mae),
                    error = function(e) NULL)
            }
            if (is.null(qf)) {
                qf <- tryCatch(methods::as(mae, "QFeatures"),
                    error = function(e) NULL)
            }
            if (is.null(qf)) {
                stop("Failed to restore QFeatures object", call. = FALSE)
            }
            if (!is.null(assay_links)) {
                try(QFeatures::assayLinks(qf) <- assay_links, silent = TRUE)
            }
            qf
        }
    )
)
