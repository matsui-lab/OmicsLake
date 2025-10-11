#' Write a table (standardized to Parquet) staged for next commit
#' @export
ol_write <- function(name, data, project = getOption("ol.project"), compression = c("zstd","snappy","uncompressed")) {
  .ol_require(c("arrow"))
  compression <- match.arg(compression)
  project <- .ol_assert_project(project, "Call ol_init() first or set options(ol.project=...).")
  pr <- .ol_proj_root(project)
  stage <- file.path(pr, "tables", name, "_stage")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  fn <- file.path(stage, "part.parquet")
  arrow::write_parquet(data, sink = fn, compression = compression)
  invisible(.ol_norm(fn))
}

#' Save an R object (RDS/QS) staged for next commit
#' @export
ol_save <- function(name, object, project = getOption("ol.project"), prefer_qs = TRUE) {
  project <- .ol_assert_project(project, "Call ol_init() first.")
  pr <- .ol_proj_root(project)
  stage <- file.path(pr, "objects", name, "_stage")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(prefer_qs) && requireNamespace("qs", quietly = TRUE)) {
    fn <- file.path(stage, "obj.qs"); qs::qsave(object, fn, preset = "high")
  } else {
    fn <- file.path(stage, "obj.rds"); saveRDS(object, fn, compress = "xz")
  }
  invisible(.ol_norm(fn))
}

#' Commit staged entries into an immutable version
#' @export
ol_commit <- function(note = "", params = list(), project = getOption("ol.project")) {
  .ol_require(c("jsonlite","digest"))
  project <- .ol_assert_project(project, "Call ol_init() first.")
  pr  <- .ol_proj_root(project)
  sid <- .ol_now_id()
  for (rootdir in c("tables","objects")) {
    rdir <- file.path(pr, rootdir)
    if (dir.exists(rdir)) {
      for (name in list.dirs(rdir, full.names = FALSE, recursive = FALSE)) {
        sdir <- file.path(rdir, name, "_stage")
        if (dir.exists(sdir)) {
          vdir <- file.path(rdir, name, paste0("v_", sid))
          dir.create(vdir, showWarnings = FALSE)
          file.copy(list.files(sdir, full.names = TRUE), vdir, recursive = TRUE)
          unlink(sdir, recursive = TRUE, force = TRUE)
        }
      }
    }
  }
  meta <- list(project=project, state_id=sid, note=note, params=params, created_at=as.character(Sys.time()))
  jsonlite::write_json(meta, file.path(pr, "meta", paste0("state_", sid, ".json")), auto_unbox=TRUE, pretty=TRUE)
  writeLines(sid, file.path(pr, "meta", "LATEST"))
  invisible(sid)
}

.ol_resolve_state <- function(ref, project) {
  pr <- .ol_proj_root(project); mdir <- file.path(pr, "meta")
  if (is.null(ref) || ref == "@latest") {
    return(readLines(file.path(mdir, "LATEST"), n = 1))
  }
  if (startsWith(ref, "@")) {
    tag <- sub("^@", "", ref)
    if (grepl("^version\\(", tag)) {
      # @version(YYYYMMDD-HHMMSS) を取り出す
      return(sub("^version\\(([^)]+)\\)$", "\\1", tag))
    } else {
      p <- file.path(mdir, paste0("LABEL_", tag))
      if (!file.exists(p)) stop("Unknown label: ", tag)
      return(readLines(p, n = 1))
    }
  }
  ref
}

#' Read table/object by name and reference
#' @export
ol_read <- function(name, ref = "@latest", project = getOption("ol.project"), collect = TRUE) {
  .ol_require(c("arrow"))
  project <- .ol_assert_project(project, "Call ol_init() first.")
  pr  <- .ol_proj_root(project); sid <- .ol_resolve_state(ref, project)
  tdir <- file.path(pr, "tables", name, paste0("v_", sid))
  if (dir.exists(tdir)) {
    ds <- arrow::open_dataset(.ol_norm(tdir), format = "parquet")
    return(if (isTRUE(collect)) as.data.frame(arrow::collect(ds)) else ds)
  }
  odir <- file.path(pr, "objects", name, paste0("v_", sid))
  if (dir.exists(odir)) {
    f <- list.files(odir, full.names = TRUE, pattern = "obj\.(qs|rds)$")
    if (!length(f)) stop("No object payload found in: ", odir)
    if (endsWith(f, ".qs")) return(qs::qread(f)) else return(readRDS(f))
  }
  stop("Not found: ", name, " @ ", sid)
}

#' @export
ol_load <- function(name, ref = "@latest", project = getOption("ol.project")) ol_read(name, ref, project)

#' @export
ol_log <- function(project = getOption("ol.project")) {
  .ol_require("jsonlite")
  project <- .ol_assert_project(project, "Call ol_init() first.")
  pr <- .ol_proj_root(project); mdir <- file.path(pr, "meta")
  js <- list.files(mdir, pattern = "^state_.*\.json$", full.names = TRUE)
  if (!length(js)) return(utils::head(data.frame()))
  entries <- lapply(js, jsonlite::read_json, simplifyVector = TRUE)
  df <- do.call(rbind, lapply(entries, function(x) data.frame(state_id=x$state_id, created_at=x$created_at, note=x$note, stringsAsFactors=FALSE)))
  df[order(df$created_at, decreasing = TRUE), ]
}
