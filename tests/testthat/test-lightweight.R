# Tests for lightweight mode (observe.R and wrap.R)

test_that("observe() tracks reads and writes", {
  # Create temp files for testing
  temp_dir <- tempdir()
  input_file <- file.path(temp_dir, "test_input.csv")
  output_file <- file.path(temp_dir, "test_output.csv")

  # Write test input
  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)

  # Observe a simple operation
  result <- observe({
    data <- read.csv(input_file)
    data$y <- data$x * 2
    write.csv(data, output_file, row.names = FALSE)
    data
  })

  expect_s3_class(result, "lake_observation")
  expect_true(basename(input_file) %in% basename(result$reads))
  expect_true(basename(output_file) %in% basename(result$writes))
  expect_true(!is.null(result$result))

  # Clean up
  unlink(input_file)
  unlink(output_file)
})

test_that("observe() can disable auto interception", {
  temp_dir <- tempdir()
  input_file <- file.path(temp_dir, "test_input_disable.csv")
  output_file <- file.path(temp_dir, "test_output_disable.csv")

  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)

  result <- observe({
    data <- read.csv(input_file)
    write.csv(data, output_file, row.names = FALSE)
  }, track_functions = character(0))

  expect_equal(length(result$reads), 0)
  expect_equal(length(result$writes), 0)

  unlink(input_file)
  unlink(output_file)
})

test_that("observe() infers lineage from event order", {
  temp_dir <- tempdir()
  input_a <- file.path(temp_dir, "order_input_a.csv")
  input_b <- file.path(temp_dir, "order_input_b.csv")
  output_a <- file.path(temp_dir, "order_output_a.csv")
  output_b <- file.path(temp_dir, "order_output_b.csv")

  write.csv(data.frame(x = 1:3), input_a, row.names = FALSE)
  write.csv(data.frame(x = 4:6), input_b, row.names = FALSE)

  result <- observe({
    a <- read.csv(input_a)
    write.csv(a, output_a, row.names = FALSE)
    b <- read.csv(input_b)
    write.csv(b, output_b, row.names = FALSE)
  })

  expect_true(any(result$lineage$parent == basename(input_a) & result$lineage$child == basename(output_a)))
  expect_true(any(result$lineage$parent == basename(input_b) & result$lineage$child == basename(output_b)))
  expect_false(any(result$lineage$parent == basename(input_b) & result$lineage$child == basename(output_a)))

  unlink(input_a)
  unlink(input_b)
  unlink(output_a)
  unlink(output_b)
})

test_that("observe_to_lake() records inferred file lineage", {
  temp_dir <- tempdir()
  input_file <- file.path(temp_dir, "test_input_lake.csv")
  output_file <- file.path(temp_dir, "test_output_lake.csv")

  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)
  lake <- Lake$new("test_observe_to_lake")

  observe_to_lake({
    data <- read.csv(input_file)
    write.csv(data, output_file, row.names = FALSE)
    nrow(data)
  }, lake = lake)

  deps <- lake$deps(paste0("file:", basename(output_file)), direction = "up")
  expect_true(paste0("file:", basename(input_file)) %in% deps$parent_name)

  unlink(input_file)
  unlink(output_file)
  unlink(file.path(path.expand("~"), ".omicslake", "test_observe_to_lake"), recursive = TRUE)
})

test_that("track_pipeline() uses default lake and can snapshot", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  use_lake("proj_track_pipeline")
  input_file <- file.path(tmpdir, "tp_input.csv")
  output_file <- file.path(tmpdir, "tp_output.csv")
  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)

  result <- track_pipeline({
    data <- read.csv(input_file)
    write.csv(data, output_file, row.names = FALSE)
    nrow(data)
  }, snapshot = "run1")

  expect_equal(result, 5)
  deps <- lake()$deps(paste0("file:", basename(output_file)), direction = "up")
  expect_true(paste0("file:", basename(input_file)) %in% deps$parent_name)

  labels <- ol_list_labels(project = "proj_track_pipeline")
  expect_true(any(labels$tag == "run1"))
})

test_that("track_script() sources legacy scripts with lineage tracking", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  input_file <- file.path(tmpdir, "script_input.csv")
  output_file <- file.path(tmpdir, "script_output.csv")
  script_file <- file.path(tmpdir, "pipeline_script.R")
  write.csv(data.frame(x = 1:4), input_file, row.names = FALSE)

  writeLines(c(
    sprintf("d <- read.csv(%s)", deparse(input_file)),
    "d$y <- d$x * 2",
    sprintf("write.csv(d, %s, row.names = FALSE)", deparse(output_file)),
    "nrow(d)"
  ), con = script_file)

  result <- track_script(script_file, project = "proj_track_script")
  expect_equal(result, 4)

  deps <- lake()$deps(paste0("file:", basename(output_file)), direction = "up")
  expect_true(paste0("file:", basename(input_file)) %in% deps$parent_name)
})

test_that("track_pipeline() can save returned result with observed dependencies", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  use_lake("proj_track_pipeline_save")
  input_file <- file.path(tmpdir, "tp_save_input.csv")
  output_file <- file.path(tmpdir, "tp_save_output.csv")
  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)

  result <- track_pipeline({
    data <- read.csv(input_file)
    write.csv(data, output_file, row.names = FALSE)
    data.frame(n = nrow(data))
  }, save_result = TRUE, result_name = "pipeline_summary", result_depends_on = "writes")

  expect_equal(result$n, 5)
  stored <- lake()$get("pipeline_summary")
  expect_equal(stored$n, 5)
  deps <- lake()$deps("pipeline_summary", direction = "up")
  expect_true(paste0("file:", basename(output_file)) %in% deps$parent_name)
})

test_that("track_script() can save script return value with default name", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  input_file <- file.path(tmpdir, "script_save_input.csv")
  output_file <- file.path(tmpdir, "script_save_output.csv")
  script_file <- file.path(tmpdir, "pipeline_script_save.R")
  write.csv(data.frame(x = 1:4), input_file, row.names = FALSE)

  writeLines(c(
    sprintf("d <- read.csv(%s)", deparse(input_file)),
    sprintf("write.csv(d, %s, row.names = FALSE)", deparse(output_file)),
    "sum(d$x)"
  ), con = script_file)

  result <- track_script(
    script_file,
    project = "proj_track_script_save",
    save_result = TRUE,
    result_depends_on = "writes"
  )
  expect_equal(result, 10)

  expect_true(lake_exists("script_result_pipeline_script_save", type = "object"))
  expect_equal(fetch("script_result_pipeline_script_save"), 10)
  deps <- lake()$deps("script_result_pipeline_script_save", direction = "up")
  expect_true(paste0("file:", basename(output_file)) %in% deps$parent_name)
})

test_that("track_pipeline() can store observation record", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  use_lake("proj_track_pipeline_observation")
  input_file <- file.path(tmpdir, "tp_obs_input.csv")
  output_file <- file.path(tmpdir, "tp_obs_output.csv")
  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)

  track_pipeline({
    data <- read.csv(input_file)
    write.csv(data, output_file, row.names = FALSE)
    nrow(data)
  }, store_observation = TRUE, observation_name = "obs_tp_run", observation_depends_on = "both")

  expect_true(lake_exists("obs_tp_run", type = "object"))
  obs <- fetch("obs_tp_run")
  expect_true(is.list(obs))
  expect_true(all(c("reads", "writes", "lineage", "read_nodes", "write_nodes", "recorded_at") %in% names(obs)))
  expect_true(basename(input_file) %in% basename(obs$reads))
  expect_true(basename(output_file) %in% basename(obs$writes))

  deps <- lake()$deps("obs_tp_run", direction = "up")
  expect_true(paste0("file:", basename(input_file)) %in% deps$parent_name)
  expect_true(paste0("file:", basename(output_file)) %in% deps$parent_name)
})

test_that("track_script() can store observation record with default name", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  input_file <- file.path(tmpdir, "script_obs_input.csv")
  output_file <- file.path(tmpdir, "script_obs_output.csv")
  script_file <- file.path(tmpdir, "pipeline_script_observation.R")
  write.csv(data.frame(x = 1:4), input_file, row.names = FALSE)

  writeLines(c(
    sprintf("d <- read.csv(%s)", deparse(input_file)),
    sprintf("write.csv(d, %s, row.names = FALSE)", deparse(output_file)),
    "sum(d$x)"
  ), con = script_file)

  result <- track_script(
    script_file,
    project = "proj_track_script_observation",
    store_observation = TRUE
  )
  expect_equal(result, 10)

  expect_true(lake_exists("script_observation_pipeline_script_observation", type = "object"))
  obs <- fetch("script_observation_pipeline_script_observation")
  expect_true(is.list(obs))
  expect_true(basename(output_file) %in% basename(obs$writes))
})

test_that("track_script() forwards source() arguments via source_args", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  input_file <- file.path(tmpdir, "script_source_args_input.csv")
  output_file <- file.path(tmpdir, "script_source_args_output.csv")
  script_file <- file.path(tmpdir, "pipeline_script_source_args.R")
  write.csv(data.frame(x = 1:4), input_file, row.names = FALSE)

  writeLines(c(
    sprintf("d <- read.csv(%s)", deparse(input_file)),
    sprintf("write.csv(d, %s, row.names = FALSE)", deparse(output_file)),
    "sum(d$x)"
  ), con = script_file)

  result <- track_script(
    script_file,
    project = "proj_track_script_source_args",
    source_args = list(print.eval = TRUE)
  )
  expect_equal(result, 10)
  expect_true(file.exists(output_file))
})

test_that("track_script() rejects reserved source_args names", {
  tmpdir <- withr::local_tempdir()
  script_file <- file.path(tmpdir, "pipeline_script_reserved_source_args.R")
  writeLines("1 + 1", con = script_file)

  expect_error(
    track_script(script_file, project = "proj_track_script_reserved", source_args = list(file = "other.R")),
    "source_args cannot include reserved names"
  )
})

test_that("transparent tracking works without wrapping code blocks", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  had_global_read <- exists("read.csv", envir = .GlobalEnv, inherits = FALSE)
  old_global_read <- if (had_global_read) get("read.csv", envir = .GlobalEnv, inherits = FALSE) else NULL

  on.exit({
    if (.auto_track_env$active) {
      ol_disable_transparent_tracking(commit = FALSE)
    }
    if (had_global_read) {
      assign("read.csv", old_global_read, envir = .GlobalEnv)
    } else if (exists("read.csv", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = "read.csv", envir = .GlobalEnv)
    }
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)

  options(ol.root = tmpdir)
  options(ol.project = NULL)

  input_file <- file.path(tmpdir, "transparent_input.csv")
  output_file <- file.path(tmpdir, "transparent_output.csv")
  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)

  ol_enable_transparent_tracking(
    project = "proj_transparent",
    auto_disable = FALSE,
    store_observation = TRUE,
    observation_name = "obs_transparent"
  )

  data <- read.csv(input_file)
  write.csv(data, output_file, row.names = FALSE)

  obs <- ol_disable_transparent_tracking(commit = TRUE)
  expect_s3_class(obs, "lake_observation")
  expect_true(basename(input_file) %in% basename(obs$reads))
  expect_true(basename(output_file) %in% basename(obs$writes))
  expect_true(lake_exists("obs_transparent", type = "object"))
  deps <- lake()$deps(paste0("file:", basename(output_file)), direction = "up")
  expect_true(paste0("file:", basename(input_file)) %in% deps$parent_name)

  if (had_global_read) {
    expect_identical(get("read.csv", envir = .GlobalEnv, inherits = FALSE), old_global_read)
  } else {
    expect_false(exists("read.csv", envir = .GlobalEnv, inherits = FALSE))
  }
})

test_that("transparent tracking guard rails work", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    if (.auto_track_env$active) {
      ol_disable_transparent_tracking(commit = FALSE)
    }
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)

  options(ol.root = tmpdir)
  options(ol.project = NULL)

  expect_s3_class(ol_disable_transparent_tracking(commit = FALSE), "lake_observation")

  ol_enable_transparent_tracking(project = "proj_transparent_guard", auto_disable = FALSE)
  expect_error(
    ol_enable_transparent_tracking(project = "proj_transparent_guard2", auto_disable = FALSE),
    "already active"
  )
  expect_no_error(ol_disable_transparent_tracking(commit = FALSE))
})

test_that("observe() returns result correctly", {
  result <- observe({
    x <- 1:10
    sum(x)
  })

  expect_equal(result$result, 55)
})

test_that("print.lake_observation works", {
  result <- observe({
    42
  })

  expect_output(print(result), "Lake Observation")
})

test_that("wrap_fn creates a working wrapper", {
  lake <- Lake$new("test_wrap_fn")

  # Simple function to wrap
  double_it <- function(x) x * 2

  # Wrap it
  tracked_double <- wrap_fn(double_it, lake, "doubled_result", save_output = FALSE)

  # Use the wrapper
  result <- tracked_double(5)
  expect_equal(result, 10)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_wrap_fn"), recursive = TRUE)
})

test_that("wrap_fn with save_output stores result", {
  lake <- Lake$new("test_wrap_save")

  add_one <- function(x) x + 1

  tracked_add <- wrap_fn(add_one, lake, "result", save_output = TRUE)
  result <- tracked_add(data.frame(val = 1:3))

  # Check that result was saved
  stored <- lake$get("result")
  expect_equal(stored$val, 2:4)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_wrap_save"), recursive = TRUE)
})

test_that("wrap_call works inline", {
  lake <- Lake$new("test_wrap_call")

  result <- wrap_call(lake, function(x) x^2, 5, output = "squared", save = TRUE)

  expect_equal(result, 25)
  expect_equal(lake$get("squared"), 25)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_wrap_call"), recursive = TRUE)
})

test_that("mark creates a marker node", {
  lake <- Lake$new("test_mark")

  # Mark some data without storing it
  large_data <- matrix(1:1000, 100, 10)
  mark("large_matrix", large_data, lake)

  # The marker should be recorded
  objects <- lake$objects()
  expect_true(any(grepl("marker", objects$name)))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_mark"), recursive = TRUE)
})

test_that("mark captures external file fingerprint metadata", {
  lake <- Lake$new("test_mark_external_metadata")
  path <- tempfile(pattern = "omicslake_mark_", fileext = ".txt")
  writeLines("alpha", con = path)

  mark("file:input.txt", path, lake)
  meta1 <- lake$get(".__marker__.file:input.txt")
  expect_equal(meta1$source_kind, "file")
  expect_true(isTRUE(meta1$source_exists))
  expect_true(is.character(meta1$content_md5))
  expect_true(nzchar(meta1$content_md5))

  Sys.sleep(1.1)
  writeLines("beta", con = path)
  mark("file:input.txt", path, lake)
  meta2 <- lake$get(".__marker__.file:input.txt")
  expect_true(is.character(meta2$content_md5))
  expect_true(nzchar(meta2$content_md5))
  expect_false(identical(meta1$content_md5, meta2$content_md5))

  unlink(file.path(path.expand("~"), ".omicslake", "test_mark_external_metadata"), recursive = TRUE)
})

test_that("mark captures API metadata from named list payload", {
  lake <- Lake$new("test_mark_api_metadata")
  meta <- list(
    source_kind = "api",
    endpoint = "https://api.example.org/v1/resource",
    etag = "v1",
    last_modified = "Sun, 22 Feb 2026 00:00:00 GMT",
    status_code = 200L,
    response_md5 = "abc123"
  )

  mark("api:example_resource", meta, lake)
  marker <- lake$get(".__marker__.api:example_resource")

  expect_equal(marker$source_kind, "api")
  expect_equal(marker$source_id, "https://api.example.org/v1/resource")
  expect_equal(marker$etag, "v1")
  expect_equal(marker$last_modified, "Sun, 22 Feb 2026 00:00:00 GMT")
  expect_equal(marker$status_code, 200L)
  expect_equal(marker$response_md5, "abc123")

  unlink(file.path(path.expand("~"), ".omicslake", "test_mark_api_metadata"), recursive = TRUE)
})

test_that("link creates explicit dependency", {
  lake <- Lake$new("test_link")

  # Create two data items
  lake$put("parent", data.frame(x = 1:3))
  lake$put("child", data.frame(y = 4:6))

  # Link them
  link("parent", "child", lake)

  # Check the dependency exists
  deps <- lake$deps("child", direction = "up")
  expect_true("parent" %in% deps$parent_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_link"), recursive = TRUE)
})

test_that("with_tracking tracks dependencies", {
  lake <- Lake$new("test_with_tracking")

  lake$put("source", data.frame(x = 1:5))

  result <- with_tracking(lake, "computed", {
    data <- lake$get("source")
    data$y <- data$x * 2
    data
  })

  expect_equal(nrow(result), 5)
  expect_true("computed" %in% lake$tables()$table_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_with_tracking"), recursive = TRUE)
})

test_that("Pipeline executes steps in order", {
  lake <- Lake$new("test_pipeline")

  pipeline <- create_pipeline(lake, "test")
  pipeline$
    step("init", function() data.frame(x = 1:5))$
    step("double", function(data) { data$x <- data$x * 2; data })$
    step("add", function(data) { data$x <- data$x + 1; data })

  result <- pipeline$run()

  expect_equal(result$x, c(3, 5, 7, 9, 11))  # (1:5) * 2 + 1

  # Check that intermediate results were stored
  expect_true("test.init" %in% lake$tables()$table_name)
  expect_true("test.double" %in% lake$tables()$table_name)
  expect_true("test.add" %in% lake$tables()$table_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_pipeline"), recursive = TRUE)
})

test_that("ObserveSession tracks operations", {
  lake <- Lake$new("test_session")

  session <- observe_session(lake)
  expect_true(session$is_active())

  # Record some operations
  session$record("put", "data1")
  session$record("get", "data1")

  summary <- session$stop()
  expect_false(session$is_active())
  expect_equal(summary$n_operations, 2)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_session"), recursive = TRUE)
})
