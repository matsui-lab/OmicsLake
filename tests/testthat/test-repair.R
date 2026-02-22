test_that("Lake$repair returns a structured five-step report", {
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

  lake <- Lake$new("proj_repair_report")
  lake$put("counts", data.frame(x = 1:3))
  lake$snap("v1")

  rep <- lake$repair(verbose = FALSE)
  expect_s3_class(rep, "lake_repair_report")
  expect_true(all(c("situation", "causes", "proposals", "execution", "comparison") %in% names(rep)))
  expect_true(is.data.frame(rep$situation))
  expect_true(is.data.frame(rep$causes))
  expect_true(is.data.frame(rep$proposals))
  expect_true(is.data.frame(rep$execution))
  expect_true(is.data.frame(rep$comparison))
  expect_true(any(rep$execution$action_id == "auto_execution"))
  expect_true(any(rep$comparison$metric == "doctor_failures"))

  out <- utils::capture.output(print(rep))
  expect_true(any(grepl("^1\\) Situation$", out)))
  expect_true(any(grepl("^5\\) Before/After Comparison$", out)))
})

test_that("Lake$repair can automatically restore a specified snapshot", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("proj_repair_restore")
  lake$put("counts", data.frame(x = 1:3))
  lake$snap("v1")

  Sys.sleep(1.1)
  lake$put("counts", data.frame(x = 10:12))

  rep <- lake$repair(auto = TRUE, restore_label = "v1", verbose = FALSE)
  expect_s3_class(rep, "lake_repair_report")
  expect_true(any(rep$execution$action_id == "restore_snapshot"))
  restore_status <- rep$execution$status[rep$execution$action_id == "restore_snapshot"][1]
  expect_equal(restore_status, "ok")

  restored <- lake$get("counts")
  expect_equal(restored$x, 1:3)
})

test_that("Lake$repair detects target value drift against restore label", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("proj_repair_target_diff")
  lake$put("counts", data.frame(x = 1:3))
  lake$snap("v1")

  Sys.sleep(1.1)
  lake$put("counts", data.frame(x = c(5, 6, 7)))

  rep <- lake$repair(target = "counts", restore_label = "v1", auto = FALSE, verbose = FALSE)
  expect_s3_class(rep, "lake_repair_report")
  expect_true("target_value_drift" %in% names(rep$situation))
  expect_true(isTRUE(rep$situation$target_value_drift[[1]]))
  expect_true(any(rep$causes$source == "target_diff"))
})

test_that("Lake$repair uses semantic comparison to avoid false drift from row order", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("proj_repair_semantic_rows")
  df <- data.frame(gene_id = c("g1", "g2", "g3"), value = c(1, 2, 3), stringsAsFactors = FALSE)
  lake$put("counts", df)
  lake$snap("v1")

  lake$put("counts", df[c(3, 1, 2), , drop = FALSE])

  rep <- lake$repair(target = "counts", restore_label = "v1", auto = FALSE, verbose = FALSE)
  expect_false(isTRUE(rep$situation$target_value_drift[[1]]))
  expect_equal(rep$situation$target_compare_mode[[1]], "semantic_table")
})

test_that("Lake$repair respects numeric_tolerance for semantic comparison", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("proj_repair_semantic_numeric")
  base <- data.frame(gene_id = c("g1", "g2", "g3"), value = c(1, 2, 3), stringsAsFactors = FALSE)
  lake$put("counts", base)
  lake$snap("v1")

  jitter <- base
  jitter$value <- jitter$value + c(1e-12, 2e-12, 3e-12)
  lake$put("counts", jitter)

  rep_loose <- lake$repair(
    target = "counts",
    restore_label = "v1",
    numeric_tolerance = 1e-8,
    auto = FALSE,
    verbose = FALSE
  )
  expect_false(isTRUE(rep_loose$situation$target_value_drift[[1]]))

  rep_strict <- lake$repair(
    target = "counts",
    restore_label = "v1",
    numeric_tolerance = 0,
    auto = FALSE,
    verbose = FALSE
  )
  expect_true(isTRUE(rep_strict$situation$target_value_drift[[1]]))
})

test_that("Lake$repair surfaces external dependency fingerprint drift", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  input_file <- file.path(tmpdir, "external_factor.txt")
  writeLines("1.0", con = input_file)

  lake <- Lake$new("proj_repair_external_fingerprint")
  mark("file:external_factor.txt", input_file, lake)
  lake$put("counts", data.frame(value = c(10, 20, 30)), depends_on = "file:external_factor.txt")
  lake$snap("v1")

  writeLines("2.0", con = input_file)
  mark("file:external_factor.txt", input_file, lake)
  lake$put("counts", data.frame(value = c(40, 50, 60)), depends_on = "file:external_factor.txt")

  rep <- lake$repair(target = "counts", restore_label = "v1", auto = FALSE, verbose = FALSE)
  expect_true(isTRUE(rep$situation$target_external_dependency_drift[[1]]))
  expect_true(any(rep$causes$source == "external_dependency"))
})

test_that("Lake$repair surfaces API dependency metadata drift", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("proj_repair_external_api")
  mark("api:service", list(
    source_kind = "api",
    source_id = "https://api.example.org/v1/service",
    etag = "e1",
    status_code = 200L,
    response_md5 = "hash1"
  ), lake)
  lake$put("counts", data.frame(value = c(10, 20, 30)), depends_on = "api:service")
  lake$snap("v1")

  mark("api:service", list(
    source_kind = "api",
    source_id = "https://api.example.org/v1/service",
    etag = "e2",
    status_code = 200L,
    response_md5 = "hash2"
  ), lake)
  lake$put("counts", data.frame(value = c(40, 50, 60)), depends_on = "api:service")

  rep <- lake$repair(target = "counts", restore_label = "v1", auto = FALSE, verbose = FALSE)
  expect_true(isTRUE(rep$situation$target_external_dependency_drift[[1]]))
  expect_true(any(rep$causes$source == "external_dependency"))
})

test_that("Lake$repair marks restore_snapshot as failed when label is missing", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("proj_repair_missing_label")
  lake$put("counts", data.frame(x = 1:3))
  lake$snap("v1")
  lake$put("counts", data.frame(x = 10:12))

  rep <- lake$repair(target = "counts", restore_label = "not_exist", auto = TRUE, verbose = FALSE)
  expect_true(any(rep$execution$action_id == "restore_snapshot"))
  st <- rep$execution$status[rep$execution$action_id == "restore_snapshot"][1]
  msg <- rep$execution$message[rep$execution$action_id == "restore_snapshot"][1]
  expect_equal(st, "failed")
  expect_match(msg, "Restore label not found")
})

test_that("lake_repair shortcut uses the default lake", {
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

  use_lake("proj_repair_shortcut")
  put("counts", data.frame(x = 1:3))

  rep <- lake_repair(verbose = FALSE)
  expect_s3_class(rep, "lake_repair_report")
  expect_true(is.data.frame(rep$situation))
})
