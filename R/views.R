#' Create a database view
#'
#' Creates a virtual table (view) based on a SQL query. Views are useful for
#' creating reusable queries, especially for comparing multiple versions of data.
#' Views do not store data but execute their query each time they are referenced.
#'
#' @param name Name for the view to create
#' @param sql SQL query defining the view (must be a SELECT statement)
#' @param project Project name (default: current project from options)
#' @param replace Whether to replace the view if it already exists (default: TRUE)
#' @param depends_on Optional character vector of table/object names that this view depends on
#'
#' @return Invisible qualified view name
#' @export
#'
#' @examples
#' \dontrun{
#' ol_init("myproject")
#' ol_write("genes", data.frame(gene_id = 1:100, expr = rnorm(100)))
#' 
#' ol_create_view("high_expr", "SELECT * FROM genes WHERE expr > 0")
#' 
#' ol_write("genes_v2", data.frame(gene_id = 1:100, expr = rnorm(100)))
#' ol_create_view("gene_comparison",
#'   "SELECT g1.gene_id, g1.expr as expr_v1, g2.expr as expr_v2,
#'    (g2.expr - g1.expr) as change
#'    FROM genes g1
#'    JOIN genes_v2 g2 ON g1.gene_id = g2.gene_id",
#'   depends_on = c("genes", "genes_v2")
#' )
#' 
#' ol_read("gene_comparison")
#' }
ol_create_view <- function(name, sql, 
                           project = getOption("ol.project"),
                           replace = TRUE,
                           depends_on = NULL) {
  .ol_validate_name(name, "view name")
  
  if (!is.character(sql) || length(sql) != 1 || !nzchar(sql)) {
    stop("sql must be a non-empty character string", call. = FALSE)
  }
  
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  conn <- state$conn
  
  DBI::dbExecute(conn, sprintf("SET search_path TO %s", state$namespace))
  
  view_ident <- .ol_sql_ident(conn, state, name)
  
  create_sql <- if (isTRUE(replace)) {
    sprintf("CREATE OR REPLACE VIEW %s AS %s", view_ident, sql)
  } else {
    sprintf("CREATE VIEW %s AS %s", view_ident, sql)
  }
  
  tryCatch({
    DBI::dbExecute(conn, create_sql)
  }, error = function(e) {
    msg <- conditionMessage(e)
    stop("Failed to create view: ", msg, call. = FALSE)
  })
  
  .ol_record_dependencies(state, name, "view", depends_on)
  
  invisible(.ol_qualified_name(state, name))
}

#' Drop a database view
#'
#' Drops a view from the project. Also removes any dependency records
#' associated with the view.
#'
#' @param name Name of the view to drop
#' @param project Project name (default: current project from options)
#'
#' @return Invisible TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' ol_init("myproject")
#' ol_create_view("my_view", "SELECT * FROM genes WHERE expr > 0")
#' 
#' ol_drop_view("my_view")
#' }
ol_drop_view <- function(name, project = getOption("ol.project")) {
  .ol_validate_name(name, "view name")
  
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  conn <- state$conn
  
  view_ident <- .ol_sql_ident(conn, state, name)
  
  drop_sql <- sprintf("DROP VIEW IF EXISTS %s", view_ident)
  
  tryCatch({
    DBI::dbExecute(conn, drop_sql)
  }, error = function(e) {
    msg <- conditionMessage(e)
    stop("Failed to drop view: ", msg, call. = FALSE)
  })
  
  .ol_ensure_dependencies_table(state)
  ident_deps <- .ol_sql_ident(conn, state, "__ol_dependencies")
  
  delete_sql <- sprintf(
    "DELETE FROM %s WHERE child_name = %s AND child_type = 'view'",
    ident_deps,
    DBI::dbQuoteString(conn, name)
  )
  
  tryCatch({
    DBI::dbExecute(conn, delete_sql)
  }, error = function(e) {
  })
  
  invisible(TRUE)
}

#' List all views in the project
#'
#' Returns a data frame with information about all views in the project,
#' including their SQL definitions and dependencies.
#'
#' @param project Project name (default: current project from options)
#'
#' @return A data frame with columns: view_name, definition, dependencies
#' @export
#'
#' @examples
#' \dontrun{
#' ol_init("myproject")
#' ol_create_view("view1", "SELECT * FROM genes WHERE expr > 0")
#' ol_create_view("view2", "SELECT * FROM genes WHERE expr < 0")
#' 
#' ol_list_views()
#' }
ol_list_views <- function(project = getOption("ol.project")) {
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  state <- .ol_get_backend_state(project)
  conn <- state$conn
  
  views_query <- sprintf(
    "SELECT table_name as view_name, view_definition as definition
     FROM information_schema.views
     WHERE table_schema = %s
     ORDER BY table_name",
    DBI::dbQuoteString(conn, state$namespace)
  )
  
  views <- tryCatch({
    DBI::dbGetQuery(conn, views_query)
  }, error = function(e) {
    data.frame(
      view_name = character(0),
      definition = character(0),
      dependencies = character(0),
      stringsAsFactors = FALSE
    )
  })
  
  if (nrow(views) == 0) {
    return(data.frame(
      view_name = character(0),
      definition = character(0),
      dependencies = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  .ol_ensure_dependencies_table(state)
  ident_deps <- .ol_sql_ident(conn, state, "__ol_dependencies")
  
  views$dependencies <- sapply(views$view_name, function(vname) {
    deps_query <- sprintf(
      "SELECT parent_name FROM %s WHERE child_name = %s AND child_type = 'view' ORDER BY created_at",
      ident_deps,
      DBI::dbQuoteString(conn, vname)
    )
    deps <- DBI::dbGetQuery(conn, deps_query)
    if (nrow(deps) == 0) return("")
    paste(deps$parent_name, collapse = ", ")
  })
  
  views
}
