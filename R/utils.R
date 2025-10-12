.ol_root <- function() getOption("ol.root", file.path(path.expand("~"), ".omicslake"))
.ol_proj_root <- function(project) file.path(.ol_root(), project)
.ol_now_id <- function() format(Sys.time(), "%Y%m%d-%H%M%S")
.ol_norm <- function(path) normalizePath(path, winslash="/", mustWork=FALSE)
.ol_assert_project <- function(project, msg) {
  if (is.null(project) || !nzchar(project)) stop(msg, call. = FALSE)
  project
}
.ol_require <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
  if (length(miss)) stop("Please install: ", paste(miss, collapse = ", "))
}

.ol_validate_name <- function(name, type = "name") {
  if (is.null(name) || !is.character(name) || !length(name) || !nzchar(name)) {
    stop(type, " must be a non-empty string", call. = FALSE)
  }
  
  if (grepl("['\";`]", name)) {
    warning(type, " '", name, "' contains special SQL characters which may cause issues", call. = FALSE)
  }
  
  invisible(TRUE)
}

.ol_validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame", call. = FALSE)
  }
  
  list_cols <- vapply(data, is.list, logical(1))
  if (any(list_cols)) {
    warning("data contains list columns which may not be supported by Arrow/Parquet: ",
            paste(names(data)[list_cols], collapse = ", "), call. = FALSE)
  }
  
  invisible(TRUE)
}
