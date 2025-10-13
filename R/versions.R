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
#' \dontrun{
#' ol_init("myproject")
#' ol_save("results", data1)
#' ol_save("results", data2)
#' ol_tag_object("results", "v1.0", when = "first")
#' 
#' ol_list_object_versions("results")
#' }
ol_list_object_versions <- function(name, project = getOption("ol.project")) {
  .ol_validate_name(name, "object name")
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  state <- .ol_get_iceberg_state(project)
  .ol_ensure_objects_table(state)
  conn <- state$conn
  
  ident_objects <- .ol_iceberg_sql_ident(conn, state, "__ol_objects")
  query <- sprintf(
    "SELECT version_ts, OCTET_LENGTH(bytes) as size_bytes FROM %s WHERE name = %s ORDER BY version_ts DESC",
    ident_objects,
    DBI::dbQuoteString(conn, name)
  )
  versions <- DBI::dbGetQuery(conn, query)
  
  if (nrow(versions) == 0) {
    return(data.frame(
      version_ts = character(0),
      tags = character(0),
      size_bytes = numeric(0),
      dependencies = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  .ol_ensure_refs_table(state)
  ident_refs <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
  ref_name <- paste0("__object__", name)
  
  tags_query <- sprintf(
    "SELECT snapshot, tag FROM %s WHERE table_name = %s",
    ident_refs,
    DBI::dbQuoteString(conn, ref_name)
  )
  tags_data <- DBI::dbGetQuery(conn, tags_query)
  
  versions$tags <- sapply(versions$version_ts, function(vts) {
    matching_tags <- tags_data$tag[tags_data$snapshot == as.character(vts)]
    if (length(matching_tags) == 0) return("")
    paste(matching_tags, collapse = ", ")
  })
  
  .ol_ensure_dependencies_table(state)
  ident_deps <- .ol_iceberg_sql_ident(conn, state, "__ol_dependencies")
  
  deps_query <- sprintf(
    "SELECT parent_name, created_at FROM %s WHERE child_name = %s ORDER BY created_at",
    ident_deps,
    DBI::dbQuoteString(conn, name)
  )
  deps_data <- DBI::dbGetQuery(conn, deps_query)
  
  versions$dependencies <- sapply(versions$version_ts, function(vts) {
    vts_time <- as.POSIXct(vts)
    matching_deps <- deps_data$parent_name[
      abs(as.numeric(difftime(as.POSIXct(deps_data$created_at), vts_time, units = "secs"))) <= 1
    ]
    if (length(matching_deps) == 0) return("")
    paste(unique(matching_deps), collapse = ", ")
  })
  
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
#' \dontrun{
#' ol_init("myproject")
#' ol_save("results", data1, depends_on = "raw")
#' ol_tag_object("results", "baseline")
#' ol_save("results", data2, depends_on = c("raw", "params"))
#' ol_tag_object("results", "improved")
#' 
#' ol_compare_versions("results")
#' 
#' ol_compare_versions("results", versions = c("baseline", "improved"))
#' }
ol_compare_versions <- function(name, versions = NULL, project = getOption("ol.project")) {
  .ol_validate_name(name, "object name")
  project <- .ol_assert_project(project, "Call ol_init_iceberg() first or set options(ol.project=...).")
  
  all_versions <- ol_list_object_versions(name, project = project)
  
  if (nrow(all_versions) == 0) {
    stop("No versions found for object: ", name, call. = FALSE)
  }
  
  if (!is.null(versions)) {
    state <- .ol_get_iceberg_state(project)
    .ol_ensure_refs_table(state)
    conn <- state$conn
    
    version_ts_list <- lapply(versions, function(v) {
      if (is.character(v) && !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v)) {
        ident_refs <- .ol_iceberg_sql_ident(conn, state, "__ol_refs")
        ref_name <- paste0("__object__", name)
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
      } else {
        return(as.character(v))
      }
    })
    
    version_ts_list <- unlist(version_ts_list[!sapply(version_ts_list, is.null)])
    
    if (length(version_ts_list) == 0) {
      stop("No valid versions found for comparison", call. = FALSE)
    }
    
    all_versions <- all_versions[as.character(all_versions$version_ts) %in% version_ts_list, ]
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
  
  all_versions$deps_added <- ""
  all_versions$deps_removed <- ""
  
  for (i in 2:nrow(all_versions)) {
    curr_deps <- strsplit(all_versions$dependencies[i], ", ")[[1]]
    prev_deps <- strsplit(all_versions$dependencies[i-1], ", ")[[1]]
    
    curr_deps <- curr_deps[nzchar(curr_deps)]
    prev_deps <- prev_deps[nzchar(prev_deps)]
    
    added <- setdiff(curr_deps, prev_deps)
    removed <- setdiff(prev_deps, curr_deps)
    
    all_versions$deps_added[i] <- if (length(added) > 0) paste(added, collapse = ", ") else ""
    all_versions$deps_removed[i] <- if (length(removed) > 0) paste(removed, collapse = ", ") else ""
  }
  
  all_versions
}
