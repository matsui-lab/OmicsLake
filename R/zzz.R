"_PACKAGE"

.onLoad <- function(libname, pkgname) {
 # Register adapters
  tryCatch({
    register_adapter(SEAdapter$new())
  }, error = function(e) {
    # Silently ignore if adapter registration fails
  })
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "OmicsLake v2.0 - Data lineage for omics analysis\n",
    "Use Lake$new('project') or use_lake('project') to get started.\n",
    "See ?Lake or vignette('lake_quickstart') for documentation."
  )
}
