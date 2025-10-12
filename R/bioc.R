#' fread-like reader for Parquet tables
#' @export
ol_fread <- function(name, ref="@latest", select=NULL, drop=NULL, nrows=Inf, filter=NULL,
                     project=getOption("ol.project"), as_tibble=FALSE) {
  .ol_require(c("arrow","dplyr"))
  project <- .ol_assert_project(project, "Call ol_init() first.")
  tbl <- ol_read(name, ref=ref, project=project, collect=TRUE)
  if (!is.null(select)) tbl <- dplyr::select(tbl, dplyr::all_of(select))
  if (!is.null(drop)) tbl <- dplyr::select(tbl, -dplyr::all_of(drop))
  if (!is.null(filter)) { expr <- rlang::parse_expr(filter); tbl <- dplyr::filter(tbl, !!expr) }
  if (!is.infinite(nrows)) tbl <- utils::head(tbl, nrows)
  if (isTRUE(as_tibble) && requireNamespace("tibble", quietly=TRUE)) tibble::as_tibble(tbl) else as.data.frame(tbl)
}

#' Build a SummarizedExperiment from long table
#' @export
ol_read_se <- function(name, ref="@latest", feature_col="feature", sample_col="sample", value_col="value",
                       project=getOption("ol.project"), backing=c("hdf5","memory")) {
  backing <- match.arg(backing)
  project <- .ol_assert_project(project, "Call ol_init() first.")
  df <- ol_read(name, ref=ref, project=project, collect=TRUE)
  feat <- factor(df[[feature_col]]); samp <- factor(df[[sample_col]])
  i <- as.integer(feat); j <- as.integer(samp); x <- as.numeric(df[[value_col]])
  mat <- Matrix::sparseMatrix(i=i, j=j, x=x, dims=c(nlevels(feat), nlevels(samp)), dimnames=list(levels(feat), levels(samp)))
  if (backing=="memory") return(SummarizedExperiment::SummarizedExperiment(assays=list(counts=mat)))
  if (!requireNamespace("HDF5Array", quietly=TRUE)) return(SummarizedExperiment::SummarizedExperiment(assays=list(counts=mat)))
  pr <- .ol_proj_root(project)
  h5 <- file.path(pr, "objects", paste0(name, "_assay.h5"))
  arr <- HDF5Array::writeHDF5Array(mat, filepath=h5, name=paste0(name, "_counts"))
  SummarizedExperiment::SummarizedExperiment(assays=list(counts=arr))
}

#' Compose a MultiAssayExperiment
#' @export
ol_read_mae <- function(assays, ref="@latest", project=getOption("ol.project"), backing=c("hdf5","memory")) {
  backing <- match.arg(backing)
  `%||%` <- function(a,b) if (!is.null(a)) a else b
  project <- .ol_assert_project(project, "Call ol_init() first.")
  if (!requireNamespace("MultiAssayExperiment", quietly=TRUE)) stop("MultiAssayExperiment required.")
  se_list <- list()
  for (nm in names(assays)) {
    cfg <- assays[[nm]]
    se_list[[nm]] <- ol_read_se(name=(cfg$name %||% nm), ref=ref, project=project, backing=backing)
  }
  MultiAssayExperiment::MultiAssayExperiment(experiments=se_list)
}
