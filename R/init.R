#' Initialize an OmicsLake project
#' @export
ol_init <- function(project, root = NULL) {
  if (!nzchar(project)) stop("project must be a non-empty string")
  if (!is.null(root)) options(ol.root = .ol_norm(root))
  pr <- .ol_proj_root(project)
  dir.create(file.path(pr, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pr, "objects"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pr, "meta"),    recursive = TRUE, showWarnings = FALSE)
  options(ol.project = project)
  invisible(.ol_norm(pr))
}

#' Label current state with a human-friendly alias
#' @export
ol_label <- function(label, state_id = NULL, project = getOption("ol.project")) {
  if (!nzchar(project)) stop("Call ol_init() first.")
  pr <- .ol_proj_root(project); mdir <- file.path(pr, "meta")
  if (is.null(state_id)) state_id <- readLines(file.path(mdir, "LATEST"), n=1)
  writeLines(state_id, file.path(mdir, paste0("LABEL_", label)))
  invisible(TRUE)
}
