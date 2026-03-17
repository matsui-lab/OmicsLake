#' @title Chromatograms Adapter
#' @description Adapter for storing and retrieving
#' \code{Chromatograms} objects.
#'
#' @details
#' This adapter currently guarantees full-fidelity roundtrip by storing the
#' complete object and manifest as internal components.
#'
#' @importFrom R6 R6Class
#' @export
ChromatogramsAdapter <- R6::R6Class("ChromatogramsAdapter",
    inherit = LakeAdapter,
    public = list(
        name = function() {
            "Chromatograms"
        },
        can_handle = function(data) {
            inherits(data, "Chromatograms")
        },
        priority = function() {
            119
        },
        put = function(lake, name, data) {
            if (!requireNamespace("Chromatograms", quietly = TRUE)) {
                stop("Package 'Chromatograms' is required for Chromatograms storage", call. = FALSE)
            }
            prefix <- paste0(name, ".__chrom__.")
            project <- lake$.__enclos_env__$private$.project
            ol_save(paste0(prefix, "object"), data, project = project)
            private$.put_manifest(prefix, data, project)
            invisible(TRUE)
        },
        get = function(lake, name, ref = "@latest") {
            prefix <- paste0(name, ".__chrom__.")
            project <- lake$.__enclos_env__$private$.project
            private$.load_manifest(prefix, name, ref, project)
            tryCatch(
                ol_read_object(paste0(prefix, "object"), ref = ref,
                    project = project),
                error = function(e) {
                    stop("Failed to restore Chromatograms object for '", name,
                        "'",
                        call. = FALSE)
                }
            )
        },
        components = function(lake, name) {
            prefix <- paste0(name, ".__chrom__.")
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
            prefix <- paste0(name, ".__chrom__.")
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
            manifest_pattern <- "\\.__chrom__\\.manifest$"
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
        .put_manifest = function(prefix, data, project) {
            manifest <- list(
                type = "Chromatograms",
                class = class(data)[1],
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
                    stop("Cannot find Chromatograms manifest for '", name, "'",
                        call. = FALSE)
                }
            )
        }
    )
)
