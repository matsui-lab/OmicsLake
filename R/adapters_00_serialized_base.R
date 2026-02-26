#' @title Serialized Adapter Helpers
#' @description Internal helper factory for object-serialization adapters.
#' @keywords internal

.ol_known_adapter_tokens <- function() {
  c(
    "se", "sce", "mae", "spectra", "qfeatures", "mse", "xcms", "chrom",
    "seurat", "spatial", "ragged", "vcf", "methyl",
    "atac", "chip", "metabol", "lipid", "glyco", "phospho",
    "transcript", "proteo", "genomics", "epigen"
  )
}

.ol_adapter_root_exclude_pattern <- function() {
  toks <- paste(.ol_known_adapter_tokens(), collapse = "|")
  paste0("\\.__(?:", toks, ")__\\.")
}

.ol_create_serialized_adapter <- function(
    class_name,
    adapter_name,
    token,
    priority,
    can_handle_fn,
    manifest_type = adapter_name,
    table_part_limit = 32L
) {
  R6::R6Class(
    class_name,
    inherit = LakeAdapter,
    public = list(
      name = function() {
        adapter_name
      },
      can_handle = can_handle_fn,
      priority = function() {
        priority
      },
      put = function(lake, name, data) {
        prefix <- private$.prefix(name)
        project <- lake$.__enclos_env__$private$.project
        ol_save(paste0(prefix, "object"), data, project = project)
        structured_core <- private$.put_structured_core(lake, prefix, data)
        table_parts <- private$.put_table_parts(prefix, data, project)
        private$.put_manifest(prefix, data, project, table_parts, structured_core)
        invisible(TRUE)
      },
      get = function(lake, name, ref = "@latest") {
        prefix <- private$.prefix(name)
        project <- lake$.__enclos_env__$private$.project
        private$.load_manifest(prefix, name, ref, project)
        tryCatch(
          ol_read_object(paste0(prefix, "object"), ref = ref, project = project),
          error = function(e) {
            stop(
              "Failed to restore ", adapter_name, " object for '", name, "'",
              call. = FALSE
            )
          }
        )
      },
      components = function(lake, name) {
        prefix <- private$.prefix(name)
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
          component = c(matching_tables$table_name, matching_objects$name),
          type = c(
            rep("table", nrow(matching_tables)),
            rep("object", nrow(matching_objects))
          ),
          stringsAsFactors = FALSE
        )
        components$component_id <- gsub(prefix_pattern, "", components$component)
        components
      },
      exists = function(lake, name) {
        objects <- tryCatch(
          lake$objects(),
          error = function(e) data.frame(name = character(0))
        )
        private$.manifest_name(name) %in% objects$name
      },
      list_names = function(lake) {
        objects <- tryCatch(
          lake$objects(),
          error = function(e) data.frame(name = character(0))
        )
        if (!("name" %in% names(objects)) || nrow(objects) == 0) {
          return(character(0))
        }
        manifest_pattern <- private$.manifest_pattern()
        manifests <- objects$name[grepl(manifest_pattern, objects$name)]
        if (!length(manifests)) {
          return(character(0))
        }
        roots <- sort(unique(sub(manifest_pattern, "", manifests)))
        roots[!grepl(.ol_adapter_root_exclude_pattern(), roots, perl = TRUE)]
      }
    ),
    private = list(
      .prefix = function(name) {
        paste0(name, ".__", token, "__.")
      },
      .manifest_name = function(name) {
        paste0(private$.prefix(name), "manifest")
      },
      .manifest_pattern = function() {
        paste0("\\.__", token, "__\\.manifest$")
      },
      .put_manifest = function(prefix, data, project, table_parts = character(0),
                               structured_core = NULL) {
        manifest <- list(
          type = manifest_type,
          class = class(data)[1],
          table_parts = as.character(table_parts),
          structured_core = if (is.null(structured_core)) NA_character_ else as.character(structured_core),
          created_at = Sys.time()
        )
        ol_save(paste0(prefix, "manifest"), manifest, project = project)
        invisible(TRUE)
      },
      .put_structured_core = function(lake, prefix, data) {
        if (!isS4(data)) {
          return(NULL)
        }

        # Prefer SCE core when available; it captures more than SE.
        if (requireNamespace("SingleCellExperiment", quietly = TRUE) &&
            methods::is(data, "SingleCellExperiment")) {
          core_name <- paste0(prefix, "core_sce")
          ok <- tryCatch({
            SCEAdapter$new()$put(lake, core_name, data)
            TRUE
          }, error = function(e) FALSE)
          if (isTRUE(ok)) {
            return(core_name)
          }
        }

        if (requireNamespace("SummarizedExperiment", quietly = TRUE) &&
            methods::is(data, "SummarizedExperiment")) {
          core_name <- paste0(prefix, "core_se")
          se_obj <- tryCatch(
            methods::as(data, "RangedSummarizedExperiment"),
            error = function(e) NULL
          )
          if (is.null(se_obj)) {
            se_obj <- tryCatch(
              methods::as(data, "SummarizedExperiment"),
              error = function(e) NULL
            )
          }
          if (!is.null(se_obj)) {
            ok <- tryCatch({
              SEAdapter$new()$put(lake, core_name, se_obj)
              TRUE
            }, error = function(e) FALSE)
            if (isTRUE(ok)) {
              return(core_name)
            }
          }
        }

        NULL
      },
      .put_table_parts = function(prefix, data, project) {
        parts <- private$.extract_table_parts(data)
        if (!length(parts)) {
          return(character(0))
        }
        saved <- character(0)
        for (part_name in names(parts)) {
          table_name <- paste0(prefix, "part.", part_name)
          ok <- tryCatch({
            ol_write(table_name, parts[[part_name]], project = project, mode = "overwrite")
            TRUE
          }, error = function(e) FALSE)
          if (isTRUE(ok)) {
            saved <- c(saved, table_name)
          }
        }
        saved
      },
      .extract_table_parts = function(data) {
        out <- list()
        private$.walk_extract_parts(
          value = data,
          path = "root",
          out = out,
          depth = 0L,
          max_depth = 4L
        )
      },
      .walk_extract_parts = function(value, path, out, depth, max_depth) {
        if (length(out) >= as.integer(table_part_limit)) {
          return(out)
        }

        df <- private$.coerce_part_to_table(value)
        if (!is.null(df)) {
          safe_key <- private$.sanitize_part_name(path)
          if (nzchar(safe_key) && is.null(out[[safe_key]])) {
            out[[safe_key]] <- df
          }
          return(out)
        }

        if (depth >= max_depth) {
          return(out)
        }

        if (is.list(value)) {
          nms <- names(value)
          if (is.null(nms)) {
            nms <- paste0("item_", seq_along(value))
          }
          for (i in seq_along(value)) {
            out <- private$.walk_extract_parts(
              value = value[[i]],
              path = paste(path, "list", nms[[i]], sep = "_"),
              out = out,
              depth = depth + 1L,
              max_depth = max_depth
            )
            if (length(out) >= as.integer(table_part_limit)) {
              break
            }
          }
        } else if (isS4(value)) {
          slot_names <- tryCatch(methods::slotNames(value), error = function(e) character(0))
          for (slot_name in slot_names) {
            slot_value <- tryCatch(methods::slot(value, slot_name), error = function(e) NULL)
            if (!is.null(slot_value)) {
              out <- private$.walk_extract_parts(
                value = slot_value,
                path = paste(path, "slot", slot_name, sep = "_"),
                out = out,
                depth = depth + 1L,
                max_depth = max_depth
              )
            }
            if (length(out) >= as.integer(table_part_limit)) {
              break
            }
          }
        }

        out
      },
      .coerce_part_to_table = function(value) {
        if (is.data.frame(value)) {
          if ((nrow(value) * max(1L, ncol(value))) > 5e6) {
            return(NULL)
          }
          if (any(vapply(value, is.list, logical(1)))) {
            return(NULL)
          }
          return(value)
        }

        if (is.matrix(value)) {
          if ((nrow(value) * max(1L, ncol(value))) > 5e6) {
            return(NULL)
          }
          df <- as.data.frame(value, stringsAsFactors = FALSE)
          row_id <- rownames(value)
          if (is.null(row_id) || length(row_id) != nrow(value)) {
            row_id <- as.character(seq_len(nrow(value)))
          }
          df$.__row_id__ <- row_id
          cols <- c(".__row_id__", setdiff(names(df), ".__row_id__"))
          return(df[, cols, drop = FALSE])
        }

        if (is.atomic(value) && is.vector(value) && is.null(dim(value)) && length(value) > 0) {
          if (length(value) > 50000) {
            return(NULL)
          }
          nm <- names(value)
          out <- data.frame(
            .__index__ = seq_along(value),
            value = as.vector(value),
            stringsAsFactors = FALSE
          )
          if (!is.null(nm) && length(nm) == length(value)) {
            out$.__name__ <- nm
          }
          return(out)
        }

        NULL
      },
      .sanitize_part_name = function(x) {
        y <- gsub("[^A-Za-z0-9_]+", "_", as.character(x)[1])
        y <- gsub("^_+|_+$", "", y)
        if (!nzchar(y)) {
          y <- "part"
        }
        y
      },
      .load_manifest = function(prefix, name, ref, project) {
        tryCatch(
          ol_read_object(paste0(prefix, "manifest"), ref = ref, project = project),
          error = function(e) {
            stop(
              "Cannot find ", adapter_name, " manifest for '", name, "'",
              call. = FALSE
            )
          }
        )
      }
    )
  )
}
