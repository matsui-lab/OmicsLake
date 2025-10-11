#' @export
vx_init <- function(project, root = NULL) {
  if (!nzchar(project)) stop("project must be a non-empty string")
  if (!is.null(root)) options(ve.root = .ve_norm(root))
  pr <- .ve_proj_root(project)
  dir.create(file.path(pr, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pr, "objects"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pr, "meta"),    recursive = TRUE, showWarnings = FALSE)
  options(ve.project = project)
  invisible(.ve_norm(pr))
}
#' @export
vx_label <- function(label, state_id = NULL, project = getOption("ve.project")) {
  if (!nzchar(project)) stop("Call vx_init() first.")
  pr <- .ve_proj_root(project); mdir <- file.path(pr, "meta")
  if (is.null(state_id)) state_id <- readLines(file.path(mdir, "LATEST"), n=1)
  writeLines(state_id, file.path(mdir, paste0("LABEL_", label)))
  invisible(TRUE)
}
