.ve_root <- function() getOption("ve.root", file.path(path.expand("~"), ".ve"))
.ve_proj_root <- function(project) file.path(.ve_root(), project)
.ve_now_id <- function() format(Sys.time(), "%Y%m%d-%H%M%S")
.ve_norm <- function(path) normalizePath(path, winslash="/", mustWork=FALSE)
.ve_require <- function(pkgs) { miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]; if (length(miss)) stop("Please install: ", paste(miss, collapse = ", ")) }
