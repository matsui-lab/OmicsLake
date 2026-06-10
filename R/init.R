#' Initialize an OmicsLake project
#' @param project Project name
#' @param root Optional storage root directory. Defaults to the OmicsLake root.
#' @param ... Additional arguments passed to the backend initializer
#' @return Invisible normalized project root path
#' @export
#' @examples
#' ol_init("ex_ol_init", root = tempfile())
ol_init <- function(project, root = NULL, ...) {
  if (is.null(project) || !nzchar(project)) {
    stop("project must be a non-empty string", call. = FALSE)
  }
  if (!is.null(root)) {
    # Create the root before normalizing so the stored path is canonical
    # (e.g. resolves the macOS /var -> /private/var symlink consistently).
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    options(ol.root = .ol_norm(root))
  }
  .ol_init_backend(project = project, ...)
  invisible(.ol_norm(.ol_proj_root(project)))
}

#' Label current state with a human-friendly alias
#' @param label Label name to assign to the current state
#' @param state_id Optional state identifier; defaults to the current state
#' @param project Project name
#' @param .in_transaction Internal parameter - if TRUE, skip transaction in
#'   child calls (caller handles it)
#' @return Invisible label name
#' @export
#' @examples
#' ol_init("ex_ol_label", root = tempfile())
#' ol_write("t", data.frame(x = 1:3))
#' ol_label("v1")
ol_label <- function(label, state_id = NULL,
                     project = getOption("ol.project"),
                     .in_transaction = FALSE) {
  .ol_validate_name(label, "label")
  project <- .ol_assert_project(
    project, "Call ol_init() first or set options(ol.project=...)."
  )
  state <- try(.ol_get_backend_state(project), silent = TRUE)
  if (inherits(state, "try-error")) return(invisible(TRUE))
  .ol_ensure_refs_table(state)
  conn <- state$conn

  ident <- .ol_sql_ident(conn, state, "__ol_refs")
  delete_sql <- sprintf(
    "DELETE FROM %s WHERE table_name = %s AND tag = %s",
    ident,
    DBI::dbQuoteString(conn, "__project__"),
    DBI::dbQuoteString(conn, label)
  )
  DBI::dbExecute(conn, delete_sql)

  insert_sql <- sprintf(
    "INSERT INTO %s (table_name, tag, snapshot, as_of) VALUES (%s, %s, %s, %s)",
    ident,
    DBI::dbQuoteString(conn, "__project__"),
    DBI::dbQuoteString(conn, label),
    DBI::dbQuoteString(
      conn,
      if (is.null(state_id)) {
        format(Sys.time(), "%Y%m%d-%H%M%S")
      } else {
        state_id
      }
    ),
    "CURRENT_TIMESTAMP"
  )
  DBI::dbExecute(conn, insert_sql)

  # Filter tables (exclude internal/backup tables)
  tables <- DBI::dbListTables(conn, DBI::Id(schema = state$namespace))
  tables <- setdiff(tables, c(
    "__ol_refs", "__ol_objects", "__ol_commits",
    "__ol_dependencies", "__ol_adapters"
  ))
  tables <- grep("^__ol_backup_", tables, value = TRUE, invert = TRUE)

  for (name in tables) {
    tryCatch({
      ol_tag(name, label, project = project, ref = "@latest",
             .in_transaction = .in_transaction)
    }, error = function(e) {
      warning("Failed to tag table '", name, "': ",
              conditionMessage(e), call. = FALSE)
    })
  }

  objects <- tryCatch({
    ol_list_objects(project = project)
  }, error = function(e) data.frame(name = character(0)))

  for (obj_name in objects$name) {
    tryCatch({
      ol_tag_object(obj_name, label, when = "latest", project = project,
                    .in_transaction = .in_transaction)
    }, error = function(e) {
      warning("Failed to tag object '", obj_name, "': ",
              conditionMessage(e), call. = FALSE)
    })
  }

  invisible(TRUE)
}
