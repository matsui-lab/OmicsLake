#' @export
vx_fread <- function(name, ref = "@latest",
                     select = NULL, drop = NULL, nrows = Inf,
                     colClasses = NULL, filter = NULL,
                     project = getOption("ve.project"), as_tibble = FALSE) {
  pr  <- .ve_proj_root(project); sid <- .ve_resolve_state(ref, project)
  dir <- file.path(pr, "tables", name, paste0("v_", sid))
  if (!dir.exists(dir)) stop("Table not found: ", name)
  ds <- arrow::open_dataset(dir, format = "parquet")
  if (!is.null(select)) { ds <- ds %>% dplyr::select(dplyr::all_of(select)) }
  if (!is.null(drop))   { ds <- ds %>% dplyr::select(-dplyr::all_of(drop)) }
  if (!is.null(filter)) { expr <- rlang::parse_expr(filter); ds <- dplyr::filter(ds, !!expr) }
  tbl <- if (is.infinite(nrows)) arrow::collect(ds) else ds %>% dplyr::slice_head(n = nrows) %>% arrow::collect()
  df <- as.data.frame(tbl)
  if (!is.null(colClasses)) for (nm in intersect(names(colClasses), names(df))) {
    cls <- colClasses[[nm]]; df[[nm]] <- switch(cls,
      integer = as.integer(df[[nm]]),
      numeric = as.numeric(df[[nm]]),
      double  = as.double(df[[nm]]),
      character = as.character(df[[nm]]),
      logical = as.logical(df[[nm]]),
      factor = as.factor(df[[nm]]), df[[nm]])
  }
  if (isTRUE(as_tibble) && requireNamespace("tibble", quietly=TRUE)) return(tibble::as_tibble(df))
  df
}
#' @export
vx_read_se <- function(name, ref = "@latest",
                       feature_col = "feature", sample_col  = "sample", value_col   = "value",
                       rowData_cols = NULL, colData_cols = NULL,
                       project = getOption("ve.project"),
                       backing = c("hdf5","memory"), h5_path = NULL) {
  backing <- match.arg(backing)
  pr  <- .ve_proj_root(project); sid <- .ve_resolve_state(ref, project)
  dir <- file.path(pr, "tables", name, paste0("v_", sid))
  if (!dir.exists(dir)) stop("Table not found for ", name)
  ds <- arrow::open_dataset(dir, format = "parquet")
  needed <- unique(c(feature_col, sample_col, value_col, rowData_cols, colData_cols))
  ds <- ds %>% dplyr::select(dplyr::all_of(needed))
  df <- as.data.frame(arrow::collect(ds))
  feat <- factor(df[[feature_col]]); samp <- factor(df[[sample_col]])
  i <- as.integer(feat); j <- as.integer(samp); x <- as.numeric(df[[value_col]])
  mat <- Matrix::sparseMatrix(i=i, j=j, x=x, dims=c(nlevels(feat), nlevels(samp)), dimnames=list(levels(feat), levels(samp)))
  rowData <- if (!is.null(rowData_cols) && length(rowData_cols)) { rd <- unique(df[, c(feature_col, rowData_cols), drop=FALSE]); rd <- rd[match(rownames(mat), rd[[feature_col]]), rowData_cols, drop=FALSE]; rownames(rd) <- rownames(mat); S4Vectors::DataFrame(rd) } else NULL
  colData <- if (!is.null(colData_cols) && length(colData_cols)) { cd <- unique(df[, c(sample_col, colData_cols), drop=FALSE]); cd <- cd[match(colnames(mat), cd[[sample_col]]), colData_cols, drop=FALSE]; rownames(cd) <- colnames(mat); S4Vectors::DataFrame(cd) } else NULL
  if (backing == "memory") return(SummarizedExperiment::SummarizedExperiment(assays=list(counts=mat), rowData=rowData, colData=colData))
  if (!requireNamespace("HDF5Array", quietly = TRUE) || !requireNamespace("DelayedArray", quietly = TRUE)) {
    warning("HDF5Array/DelayedArray not installed; returning in-memory SE."); return(SummarizedExperiment::SummarizedExperiment(assays=list(counts=mat), rowData=rowData, colData=colData))
  }
  if (is.null(h5_path)) h5_path <- file.path(pr, "objects", paste0(name, "_assay.h5"))
  h5 <- HDF5Array::writeHDF5Array(mat, filepath=h5_path, name=paste0(name, "_counts"))
  SummarizedExperiment::SummarizedExperiment(assays=list(counts=h5), rowData=rowData, colData=colData)
}
#' @export
vx_read_mae <- function(assays, ref = "@latest",
                        key_cols = list(feature="feature", sample="sample", value="value"),
                        project = getOption("ve.project"),
                        backing = c("hdf5","memory")) {
  backing <- match.arg(backing)
  `%||%` <- function(a,b) if (!is.null(a)) a else b
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) stop("MultiAssayExperiment is required.")
  se_list <- list()
  for (nm in names(assays)) {
    cfg <- assays[[nm]]
    se_list[[nm]] <- vx_read_se(name = (cfg$name %||% nm), ref = ref,
                                feature_col = (cfg$feature %||% key_cols$feature),
                                sample_col  = (cfg$sample  %||% key_cols$sample),
                                value_col   = (cfg$value   %||% key_cols$value),
                                rowData_cols = cfg$rowData,
                                colData_cols = cfg$colData,
                                project = project, backing = backing)
  }
  MultiAssayExperiment::MultiAssayExperiment(experiments = se_list)
}
