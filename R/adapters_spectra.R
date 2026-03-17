#' @title Spectra Adapter
#' @description Adapter for storing and retrieving
#' \code{Spectra} objects.
#'
#' @details
#' This adapter preserves \code{Spectra} components:
#' \itemize{
#'   \item peak lists (\code{peaksData})
#'   \item per-spectrum metadata (\code{spectraData})
#' }
#'
#' @importFrom R6 R6Class
#' @export
SpectraAdapter <- R6::R6Class("SpectraAdapter",
    inherit = LakeAdapter,
    public = list(
        name = function() {
            "Spectra"
        },
        can_handle = function(data) {
            inherits(data, "Spectra")
        },
        priority = function() {
            115
        },
        put = function(lake, name, data) {
            if (!requireNamespace("Spectra", quietly = TRUE)) {
                stop("Spectra package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__spectra__.")
            project <- lake$.__enclos_env__$private$.project

            spectra_data_mode <- private$.put_spectra_data(prefix, data, project)
            private$.put_peaks_data(prefix, data, project)
            private$.put_manifest(prefix, data, spectra_data_mode,
                project)
            invisible(TRUE)
        },
        get = function(lake, name, ref = "@latest") {
            if (!requireNamespace("Spectra", quietly = TRUE)) {
                stop("Spectra package is required", call. = FALSE)
            }
            prefix <- paste0(name, ".__spectra__.")
            project <- lake$.__enclos_env__$private$.project
            manifest <- private$.load_manifest(prefix, name, ref, project)

            spectra_data <- private$.load_spectra_data(prefix, manifest, ref,
                project)
            peaks <- private$.load_peaks_data(prefix, manifest, ref, project)
            private$.build_spectra(peaks, spectra_data)
        },
        components = function(lake, name) {
            prefix <- paste0(name, ".__spectra__.")
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
            prefix <- paste0(name, ".__spectra__.")
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
            manifest_pattern <- "\\.__spectra__\\.manifest$"
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
        .put_spectra_data = function(prefix, data, project) {
            spectra_data <- as.data.frame(Spectra::spectraData(data))
            spectra_data$.__spectrum_index__ <- seq_len(nrow(spectra_data))
            has_list_like <- any(vapply(spectra_data, function(x) {
                is.list(x) || inherits(x, "List")
            }, logical(1)))
            if (has_list_like) {
                ol_save(paste0(prefix, "spectraData.object"), spectra_data,
                    project = project)
                return("object")
            }
            ol_write(paste0(prefix, "spectraData"), spectra_data,
                project = project, mode = "overwrite")
            "table"
        },
        .put_peaks_data = function(prefix, data, project) {
            peaks <- Spectra::peaksData(data)
            ol_save(paste0(prefix, "peaksData"), peaks, project = project)
            invisible(TRUE)
        },
        .put_manifest = function(prefix, data, spectra_data_mode, project) {
            backend_class <- tryCatch(
                class(Spectra::backend(data))[1],
                error = function(e) NA_character_
            )
            manifest <- list(
                type = "Spectra",
                class = class(data)[1],
                n_spectra = as.integer(length(data)),
                spectra_data_mode = as.character(spectra_data_mode),
                backend_class = as.character(backend_class),
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
                    stop("Cannot find Spectra manifest for '", name, "'",
                        call. = FALSE)
                }
            )
        },
        .load_spectra_data = function(prefix, manifest, ref, project) {
            mode <- private$.manifest_scalar(manifest$spectra_data_mode,
                "table")
            spectra_data <- if (identical(mode, "object")) {
                tryCatch(
                    ol_read_object(paste0(prefix, "spectraData.object"),
                        ref = ref, project = project),
                    error = function(e) NULL
                )
            } else {
                tryCatch(
                    ol_read(paste0(prefix, "spectraData"), ref = ref,
                        project = project),
                    error = function(e) NULL
                )
            }
            if (is.null(spectra_data)) {
                n <- private$.manifest_n_spectra(manifest)
                spectra_data <- data.frame(
                    .__spectrum_index__ = seq_len(n),
                    stringsAsFactors = FALSE
                )
            }
            if (".__spectrum_index__" %in% names(spectra_data)) {
                ord <- order(as.integer(spectra_data$.__spectrum_index__))
                spectra_data <- spectra_data[ord, , drop = FALSE]
                spectra_data$.__spectrum_index__ <- NULL
            }
            spectra_data
        },
        .load_peaks_data = function(prefix, manifest, ref, project) {
            peaks <- tryCatch(
                ol_read_object(paste0(prefix, "peaksData"), ref = ref,
                    project = project),
                error = function(e) NULL
            )
            if (!is.null(peaks)) {
                return(peaks)
            }
            n <- private$.manifest_n_spectra(manifest)
            vector("list", n)
        },
        .build_spectra = function(peaks, spectra_data) {
            data <- as.data.frame(spectra_data)
            n <- nrow(data)
            if (!n) {
                data <- data.frame()
            }
            if (length(peaks) < n) {
                peaks <- c(peaks, vector("list", n - length(peaks)))
            } else if (length(peaks) > n) {
                peaks <- peaks[seq_len(n)]
            }
            mz <- vector("list", n)
            intensity <- vector("list", n)
            if (n > 0) {
                for (i in seq_len(n)) {
                    parsed <- private$.parse_peak_matrix(peaks[[i]])
                    mz[[i]] <- parsed$mz
                    intensity[[i]] <- parsed$intensity
                }
            }
            data$mz <- I(mz)
            data$intensity <- I(intensity)
            backend <- Spectra::backendInitialize(
                Spectra::MsBackendMemory(),
                data = data
            )
            Spectra::Spectra(backend)
        },
        .parse_peak_matrix = function(x) {
            if (is.null(x) || !length(x)) {
                return(list(mz = numeric(0), intensity = numeric(0)))
            }
            mat <- tryCatch(as.matrix(x), error = function(e) NULL)
            if (is.null(mat) || !nrow(mat)) {
                return(list(mz = numeric(0), intensity = numeric(0)))
            }
            cn <- colnames(mat)
            mz_col <- if (!is.null(cn) && "mz" %in% cn) {
                which(cn == "mz")[1]
            } else {
                1L
            }
            int_col <- if (!is.null(cn) && "intensity" %in% cn) {
                which(cn == "intensity")[1]
            } else if (ncol(mat) >= 2) {
                2L
            } else {
                NA_integer_
            }
            mz <- suppressWarnings(as.numeric(mat[, mz_col]))
            intensity <- if (!is.na(int_col)) {
                suppressWarnings(as.numeric(mat[, int_col]))
            } else {
                rep(NA_real_, length(mz))
            }
            list(mz = mz, intensity = intensity)
        },
        .manifest_scalar = function(x, default = "") {
            vals <- as.character(x)
            if (!length(vals) || is.na(vals[[1]])) {
                return(default)
            }
            vals[[1]]
        },
        .manifest_n_spectra = function(manifest) {
            n <- suppressWarnings(as.integer(manifest$n_spectra))
            if (is.na(n) || n < 0L) {
                return(0L)
            }
            n
        }
    )
)
