#' List all versions of an object
#'
#' Returns a data frame with all saved versions of an object, including
#' timestamps, tags, size, and dependencies for each version.
#'
#' @param name Name of the object to list versions for
#' @param project Project name
#' @return A data frame with columns: version_ts, tags, size_bytes, dependencies
#' @export
#'
#' @examples
#' if (FALSE) {
#'     ol_init("myproject")
#'     ol_save("results", data1)
#'     ol_save("results", data2)
#'     ol_tag_object("results", "v1.0", when = "first")
#'
#'     ol_list_object_versions("results")
#' }
.ol_empty_object_versions <- function() {
    data.frame(
        version_ts = character(0),
        tags = character(0),
        size_bytes = numeric(0),
        dependencies = character(0),
        stringsAsFactors = FALSE
    )
}

.ol_version_tag_map <- function(state, name) {
    conn <- state$conn
    ident_refs <- .ol_sql_ident(conn, state, "__ol_refs")
    ref_name <- paste0("__object__", name)
    tags_query <- sprintf(
        "SELECT snapshot, tag FROM %s WHERE table_name = %s",
        ident_refs,
        DBI::dbQuoteString(conn, ref_name)
    )
    DBI::dbGetQuery(conn, tags_query)
}

.ol_version_dependency_map <- function(state, name, versions) {
    conn <- state$conn
    .ol_ensure_dependencies_table(state)
    ident_deps <- .ol_sql_ident(conn, state, "__ol_dependencies")
    deps_query <- sprintf(
        paste(
            "SELECT parent_name, created_at FROM %s",
            "WHERE child_name = %s ORDER BY created_at"
        ),
        ident_deps,
        DBI::dbQuoteString(conn, name)
    )
    deps_data <- DBI::dbGetQuery(conn, deps_query)
    version_deps <- stats::setNames(
        vector("list", nrow(versions)),
        as.character(versions$version_ts)
    )
    if (nrow(deps_data) == 0) {
        return(version_deps)
    }
    version_times <- as.POSIXct(versions$version_ts)
    for (i in seq_len(nrow(deps_data))) {
        dep_time <- as.POSIXct(deps_data$created_at[i])
        distances <- abs(as.numeric(difftime(version_times, dep_time,
            units = "secs")))
        idx <- which.min(distances)
        key <- as.character(versions$version_ts[idx])
        version_deps[[key]] <- c(version_deps[[key]], deps_data$parent_name[i])
    }
    version_deps
}

ol_list_object_versions <- function(name, project = getOption("ol.project")) {
    .ol_validate_name(name, "object name")
    project <- .ol_assert_project(project,
        "Call ol_init() first or set options(ol.project=...).")
    state <- .ol_get_backend_state(project)
    .ol_ensure_objects_table(state)
    conn <- state$conn
    ident_objects <- .ol_sql_ident(conn, state, "__ol_objects")
    query <- sprintf(
        paste(
            "SELECT version_ts, OCTET_LENGTH(bytes) as size_bytes FROM %s",
            "WHERE name = %s ORDER BY version_ts DESC"
        ),
        ident_objects,
        DBI::dbQuoteString(conn, name)
    )
    versions <- DBI::dbGetQuery(conn, query)

    if (nrow(versions) == 0) {
        return(.ol_empty_object_versions())
    }
    .ol_ensure_refs_table(state)
    tags_data <- .ol_version_tag_map(state, name)
    versions$tags <- vapply(versions$version_ts, function(vts) {
        matching_tags <- tags_data$tag[tags_data$snapshot == as.character(vts)]
        if (length(matching_tags) == 0) {
            return("")
        }
        paste(matching_tags, collapse = ", ")
    }, character(1))

    version_deps <- .ol_version_dependency_map(state, name, versions)
    versions$dependencies <- vapply(versions$version_ts, function(vts) {
        deps <- unique(version_deps[[as.character(vts)]])
        if (length(deps) == 0) {
            return("")
        }
        paste(deps, collapse = ", ")
    }, character(1))
    versions
}

#' Compare versions of an object
#'
#' Shows a side-by-side comparison of multiple versions of an object,
#' including timestamps, tags, size changes, and dependency changes.
#'
#' @param name Name of the object to compare versions for
#' @param versions Optional vector of version identifiers (timestamps or tags).
#'   If NULL, compares all versions.
#' @param project Project name
#' @return A data frame comparing the specified versions
#' @export
#'
#' @examples
#' if (FALSE) {
#'     ol_init("myproject")
#'     ol_save("results", data1, depends_on = "raw")
#'     ol_tag_object("results", "baseline")
#'     ol_save("results", data2, depends_on = c("raw", "params"))
#'     ol_tag_object("results", "improved")
#'
#'     ol_compare_versions("results")
#'
#'     ol_compare_versions("results", versions = c("baseline", "improved"))
#' }
.ol_resolve_version_filter <- function(state, name, versions) {
    conn <- state$conn
    ident_refs <- .ol_sql_ident(conn, state, "__ol_refs")
    ref_name <- paste0("__object__", name)
    resolved <- lapply(versions, function(v) {
        if (is.character(v) && !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v)) {
            query <- sprintf(
                "SELECT snapshot FROM %s WHERE table_name = %s AND tag = %s",
                ident_refs,
                DBI::dbQuoteString(conn, ref_name),
                DBI::dbQuoteString(conn, v)
            )
            res <- DBI::dbGetQuery(conn, query)
            if (nrow(res) == 0) {
                warning("Tag not found: ", v, call. = FALSE)
                return(NULL)
            }
            return(res$snapshot[1])
        }
        as.character(v)
    })
    unlist(resolved[!vapply(resolved, is.null, logical(1))])
}

.ol_compare_dependency_delta <- function(versions_df) {
    versions_df$deps_added <- ""
    versions_df$deps_removed <- ""
    for (i in seq_len(nrow(versions_df) - 1L)) {
        curr_deps <- strsplit(versions_df$dependencies[i], ", ")[[1]]
        prev_deps <- strsplit(versions_df$dependencies[i + 1], ", ")[[1]]
        curr_deps <- curr_deps[nzchar(curr_deps)]
        prev_deps <- prev_deps[nzchar(prev_deps)]
        added <- setdiff(curr_deps, prev_deps)
        removed <- setdiff(prev_deps, curr_deps)
        versions_df$deps_added[i] <- if (length(added) > 0) paste(added,
            collapse = ", ") else ""
        versions_df$deps_removed[i] <- if (length(removed) > 0) paste(removed,
            collapse = ", ") else ""
    }
    versions_df
}

ol_compare_versions <- function(name, versions = NULL,
    project = getOption("ol.project")) {
    .ol_validate_name(name, "object name")
    project <- .ol_assert_project(project,
        "Call ol_init() first or set options(ol.project=...).")
    all_versions <- ol_list_object_versions(name, project = project)
    if (nrow(all_versions) == 0) {
        stop("No versions found for object: ", name, call. = FALSE)
    }
    if (!is.null(versions)) {
        state <- .ol_get_backend_state(project)
        .ol_ensure_refs_table(state)
        version_ts_list <- .ol_resolve_version_filter(state, name, versions)
        if (length(version_ts_list) == 0) {
            stop("No valid versions found for comparison", call. = FALSE)
        }
        all_versions <- all_versions[
            as.character(all_versions$version_ts) %in% version_ts_list,
            ]
    }
    if (nrow(all_versions) < 2) {
        message("Only one version available, nothing to compare")
        return(all_versions)
    }

    all_versions$size_change <- c(NA, diff(all_versions$size_bytes))
    all_versions$time_since_previous <- c(
        NA,
        as.numeric(diff(as.POSIXct(all_versions$version_ts)), units = "hours")
    )
    .ol_compare_dependency_delta(all_versions)
}
