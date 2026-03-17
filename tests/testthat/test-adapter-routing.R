# Tests for generic adapter routing/registration in Lake$put/get/tag/drop

make_mock_adapter <- function() {
  R6::R6Class(
    "MockOmicsAdapter",
    inherit = LakeAdapter,
    public = list(
      name = function() {
        "MockOmics"
      },
      can_handle = function(data) {
        inherits(data, "mock_omics_obj")
      },
      priority = function() {
        9999
      },
      put = function(lake, name, data) {
        project <- lake$.__enclos_env__$private$.project
        prefix <- paste0(name, ".__mock__.")
        ol_save(paste0(prefix, "payload"), data, project = project)
        ol_save(
          paste0(prefix, "manifest"),
          list(type = "mock", created_at = Sys.time()),
          project = project
        )
        invisible(TRUE)
      },
      get = function(lake, name, ref = "@latest") {
        project <- lake$.__enclos_env__$private$.project
        prefix <- paste0(name, ".__mock__.")
        payload <- ol_read_object(paste0(prefix, "payload"), ref = ref, project = project)
        structure(payload, class = "mock_omics_obj")
      },
      components = function(lake, name) {
        prefix <- paste0(name, ".__mock__.")
        data.frame(
          component = c(
            paste0(prefix, "payload"),
            paste0(prefix, "manifest")
          ),
          component_id = c("payload", "manifest"),
          type = c("object", "object"),
          stringsAsFactors = FALSE
        )
      },
      exists = function(lake, name) {
        prefix <- paste0(name, ".__mock__.")
        objects <- tryCatch(
          lake$objects(),
          error = function(e) data.frame(name = character(0))
        )
        paste0(prefix, "manifest") %in% objects$name
      }
    )
  )$new()
}

test_that("Lake$put routes object storage through registered adapters", {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  register_adapter(make_mock_adapter())

  lake <- Lake$new("test_adapter_object_routing")
  obj <- structure(list(value = 1L), class = "mock_omics_obj")
  lake$put("mock_data", obj)

  restored <- lake$get("mock_data")
  expect_true(inherits(restored, "mock_omics_obj"))
  expect_equal(restored$value, 1L)

  state <- .ol_get_backend_state("test_adapter_object_routing")
  info <- .ol_get_adapter_info(state, "mock_data")
  expect_false(is.null(info))
  expect_equal(info$adapter_name, "MockOmics")
  expect_true(any(grepl("payload", info$components)))
})

test_that("Lake$get falls back to marker-based adapter detection without registry row", {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  register_adapter(make_mock_adapter())

  lake <- Lake$new("test_adapter_registry_fallback")
  obj <- structure(list(value = 7L), class = "mock_omics_obj")
  lake$put("mock_data", obj)

  state <- .ol_get_backend_state("test_adapter_registry_fallback")
  conn <- state$conn
  ident <- .ol_sql_ident(conn, state, "__ol_adapters")
  DBI::dbExecute(
    conn,
    sprintf(
      "DELETE FROM %s WHERE name = %s",
      ident,
      DBI::dbQuoteString(conn, "mock_data")
    )
  )

  expect_null(.ol_get_adapter_info(state, "mock_data"))

  restored <- lake$get("mock_data")
  expect_true(inherits(restored, "mock_omics_obj"))
  expect_equal(restored$value, 7L)
})

test_that("Adapter-managed objects support tag refs and drop via generic component resolution", {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  register_adapter(make_mock_adapter())

  lake <- Lake$new("test_adapter_tag_drop")

  lake$put("mock_data", structure(list(value = 1L), class = "mock_omics_obj"))
  lake$tag("mock_data", "v1")

  lake$put("mock_data", structure(list(value = 2L), class = "mock_omics_obj"))
  lake$tag("mock_data", "v2")

  v1 <- lake$get("mock_data", ref = "@tag(v1)")
  v2 <- lake$get("mock_data", ref = "@tag(v2)")
  expect_equal(v1$value, 1L)
  expect_equal(v2$value, 2L)

  lake$drop("mock_data")
  objects <- lake$objects()
  expect_false(any(grepl("^mock_data\\.__mock__\\.", objects$name)))

  state <- .ol_get_backend_state("test_adapter_tag_drop")
  expect_null(.ol_get_adapter_info(state, "mock_data"))
})
