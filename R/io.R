#' @export
vx_write <- function(name, data, project = getOption("ve.project"), compression = c("zstd","snappy","uncompressed")) {
  compression <- match.arg(compression)
  if (!nzchar(project)) stop("Call vx_init() first or set options(ve.project=...).")
  pr <- .ve_proj_root(project)
  stage <- file.path(pr, "tables", name, "_stage")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  fn <- file.path(stage, "part.parquet")
  arrow::write_parquet(data, sink = fn, compression = compression)
  invisible(.ve_norm(fn))
}
#' @export
vx_save <- function(name, object, project = getOption("ve.project"), prefer_qs = TRUE) {
  pr <- .ve_proj_root(project)
  stage <- file.path(pr, "objects", name, "_stage")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(prefer_qs) && requireNamespace("qs", quietly = TRUE)) {
    fn <- file.path(stage, "obj.qs"); qs::qsave(object, fn, preset = "high")
  } else {
    fn <- file.path(stage, "obj.rds"); saveRDS(object, fn, compress = "xz")
  }
  invisible(.ve_norm(fn))
}
#' @export
vx_commit <- function(note = "", params = list(), project = getOption("ve.project")) {
  pr  <- .ve_proj_root(project)
  sid <- .ve_now_id()
  troot <- file.path(pr, "tables")
  if (dir.exists(troot)) for (name in list.dirs(troot, full.names = FALSE, recursive = FALSE)) {
    sdir <- file.path(troot, name, "_stage")
    if (dir.exists(sdir)) { vdir <- file.path(troot, name, paste0("v_", sid)); dir.create(vdir, showWarnings = FALSE); file.copy(list.files(sdir, full.names = TRUE), vdir, recursive = TRUE); unlink(sdir, recursive = TRUE, force = TRUE) }
  }
  oroot <- file.path(pr, "objects")
  if (dir.exists(oroot)) for (name in list.dirs(oroot, full.names = FALSE, recursive = FALSE)) {
    sdir <- file.path(oroot, name, "_stage")
    if (dir.exists(sdir)) { vdir <- file.path(oroot, name, paste0("v_", sid)); dir.create(vdir, showWarnings = FALSE); file.copy(list.files(sdir, full.names = TRUE), vdir, recursive = TRUE); unlink(sdir, recursive = TRUE, force = TRUE) }
  }
  jsonlite::write_json(list(project=project, state_id=sid, note=note, params=params, created_at=as.character(Sys.time())), file.path(pr, "meta", paste0("state_", sid, ".json")), auto_unbox=TRUE, pretty=TRUE)
  writeLines(sid, file.path(pr, "meta", "LATEST"))
  invisible(sid)
}
.ve_resolve_state <- function(ref, project) {
  pr <- .ve_proj_root(project); mdir <- file.path(pr, "meta")
  if (is.null(ref) || ref == "@latest") return(readLines(file.path(mdir, "LATEST"), n=1))
  if (startsWith(ref, "@")) { tag <- sub("^@", "", ref); if (grepl("^version\(", tag)) return(sub("^version\(([^)]+)\)$","\1",tag)); p <- file.path(mdir, paste0("LABEL_", tag)); if (!file.exists(p)) stop("Unknown label: ", tag); return(readLines(p, n=1)) }
  ref
}
#' @export
vx_read <- function(name, ref = "@latest", project = getOption("ve.project"), collect = TRUE) {
  pr  <- .ve_proj_root(project); sid <- .ve_resolve_state(ref, project)
  tdir <- file.path(pr, "tables", name, paste0("v_", sid))
  if (dir.exists(tdir)) { ds <- arrow::open_dataset(.ve_norm(tdir), format = "parquet"); return(if (isTRUE(collect)) as.data.frame(arrow::collect(ds)) else ds) }
  odir <- file.path(pr, "objects", name, paste0("v_", sid))
  if (dir.exists(odir)) { f <- list.files(odir, full.names = TRUE, pattern = "obj\.(qs|rds)$"); if (!length(f)) stop("No object payload found"); if (endsWith(f, ".qs")) { return(qs::qread(f)) } else { return(readRDS(f)) } }
  stop("Not found: ", name, " @ ", sid)
}
#' @export
vx_load <- function(name, ref = "@latest", project = getOption("ve.project")) vx_read(name, ref, project)
#' @export
vx_log <- function(project = getOption("ve.project")) {
  pr <- .ve_proj_root(project); mdir <- file.path(pr, "meta")
  js <- list.files(mdir, pattern = "^state_.*\.json$", full.names = TRUE)
  if (!length(js)) return(utils::head(data.frame()))
  entries <- lapply(js, jsonlite::read_json, simplifyVector = TRUE)
  df <- do.call(rbind, lapply(entries, function(x) data.frame(state_id = x$state_id, created_at = x$created_at, note = x$note, stringsAsFactors = FALSE)))
  df[order(df$created_at, decreasing = TRUE), ]
}
