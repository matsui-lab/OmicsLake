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
