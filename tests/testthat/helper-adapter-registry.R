# Test helper: keep the global adapter registry from leaking between files.
#
# A few test files (adapter-routing, omics-layer-adapters) deliberately call
# clear_adapters()/register_adapter() with mock adapters. Because the registry
# (.adapter_registry) is a package-level environment, a file that ends with mock
# adapters registered (or the registry cleared) silently breaks every later file
# that relies on the real built-in SE/SCE/MAE/... adapters being present.
#
# reset_adapter_registry() restores the canonical state: it empties the registry
# and forces a fresh autoload of the real built-in adapters. Call it from a
# file-level teardown so the next test file always starts clean.
reset_adapter_registry <- function() {
  clear_adapters()   # empties registry, resets autoload_attempted to FALSE
  get_adapters()     # triggers .ol_autoregister_builtin_adapters()
  invisible(TRUE)
}

# Begin a test that installs mock adapters: clear the registry now and schedule a
# restore of the real built-ins when the calling test_that() block exits. This
# keeps mock adapters scoped to a single test and prevents cross-test/-file
# leakage. Call it instead of bare clear_adapters() at the top of such a test.
local_mock_adapters <- function(.local_envir = parent.frame()) {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE
  withr::defer(reset_adapter_registry(), envir = .local_envir)
  invisible(TRUE)
}
