"_PACKAGE"

.onLoad <- function(libname, pkgname) {
    if (exists(".adapter_registry", inherits = TRUE)) {
        .adapter_registry$autoload_attempted <- FALSE
    }
    try(.ol_autoregister_builtin_adapters(), silent = TRUE)
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        "OmicsLake v2.0 - Data lineage for omics analysis\n",
        "Use Lake$new('project') or use_lake('project') to get started.\n",
        "See ?Lake or vignette('lake_quickstart') for documentation."
    )
}
