test_that("use_lake validates project names and lake() error includes suggestions", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  on.exit(options(ol.root = old_root), add = TRUE)
  options(ol.root = tmpdir)

  if (exists("default", envir = .lake_env, inherits = FALSE)) {
    rm("default", envir = .lake_env)
  }

  expect_error(
    use_lake(""),
    "project must be a non-empty character string"
  )

  # Create a project directory so lake() can suggest it
  ol_init("proj_a")
  if (exists("default", envir = .lake_env, inherits = FALSE)) {
    rm("default", envir = .lake_env)
  }
  options(ol.project = NULL)

  expect_error(
    lake(),
    "Available projects under"
  )
})

test_that("use_lake() can recover from ol.project when default is unset", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  ol_init("proj_opt")
  options(ol.project = "proj_opt")
  if (exists("default", envir = .lake_env, inherits = FALSE)) {
    rm("default", envir = .lake_env)
  }

  l <- use_lake()
  expect_s3_class(l, "Lake")
  expect_equal(l$.__enclos_env__$private$.project, "proj_opt")
})

test_that("lake_status() and lake_doctor() work for default and explicit projects", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  use_lake("proj_diag")
  put("tbl", data.frame(x = 1:3))

  st <- lake_status()
  expect_equal(st$project, "proj_diag")
  expect_true(st$default_shortcuts)
  expect_true(st$tables >= 1)

  rep_default <- lake_doctor(verbose = FALSE)
  expect_true(all(c("check", "ok", "detail", "fix") %in% names(rep_default)))
  expect_true(any(rep_default$check == "duckdb connection is valid"))

  rep_explicit <- lake_doctor(project = "proj_diag", verbose = FALSE)
  expect_true(is.data.frame(rep_explicit))
  expect_true(nrow(rep_explicit) >= 5)
})

test_that("lake_exists() and lake_find() provide default-lake discovery shortcuts", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  options(ol.root = tmpdir)

  use_lake("proj_discovery")
  put("counts", data.frame(x = 1:3))
  put("meta_tbl", data.frame(sample = letters[1:3]))
  put("meta_obj", list(version = 1))

  expect_true(lake_exists("counts"))
  expect_true(lake_exists("meta_obj", type = "object"))
  expect_false(lake_exists("meta_obj", type = "table"))

  matches <- lake_find("meta", type = "any")
  expect_true(is.data.frame(matches))
  expect_true(nrow(matches) >= 2)
  expect_true(all(c("name", "type", "score", "distance", "match_type") %in% names(matches)))

  thresholded <- lake_find("meta", min_score = 800)
  expect_true(all(thresholded$score >= 800))

  prefer_table <- lake_find("meta_", fixed = TRUE, prefer_type = "table")
  prefer_object <- lake_find("meta_", fixed = TRUE, prefer_type = "object")
  expect_equal(prefer_table$type[1], "table")
  expect_equal(prefer_object$type[1], "object")
})

test_that("lake_* aliases provide a simple consistent API", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    if (.auto_track_env$active) {
      lake_auto_off(commit = FALSE)
    }
    options(ol.root = old_root)
    options(ol.project = old_proj)
    if (exists("default", envir = .lake_env, inherits = FALSE)) {
      rm("default", envir = .lake_env)
    }
  }, add = TRUE)
  options(ol.root = tmpdir)
  options(ol.project = NULL)

  l <- lake_use("proj_alias")
  expect_s3_class(l, "Lake")

  lake_put("counts", data.frame(x = 1:3))
  expect_equal(lake_get("counts")$x, 1:3)
  expect_true(lake_has("counts"))
  expect_false(lake_has("counts", type = "object"))
  expect_s3_class(lake_ref("counts"), "lake_tbl")

  lake_snap("v1")
  expect_no_error(lake_tag("counts", "raw"))
  expect_true(is.data.frame(lake_tree("counts")))

  input_file <- file.path(tmpdir, "alias_input.csv")
  output_file <- file.path(tmpdir, "alias_output.csv")
  write.csv(data.frame(x = 1:3), input_file, row.names = FALSE)

  lake_track({
    d <- read.csv(input_file)
    write.csv(d, output_file, row.names = FALSE)
    nrow(d)
  }, store_observation = TRUE, observation_name = "obs_alias")
  expect_true(lake_has("obs_alias", type = "object"))

  expect_no_error(lake_auto_on(project = "proj_alias", auto_disable = FALSE, store_observation = FALSE))
  expect_true(.auto_track_env$active)
  obs <- lake_auto_off(commit = FALSE)
  expect_s3_class(obs, "lake_observation")
})

test_that("lake_strict_on enables strict reproducibility mode via shortcut", {
  tmpdir <- withr::local_tempdir()
  opt_names <- c(
    "ol.repro.capture",
    "ol.repro.strict",
    "ol.repro.require_clean_git",
    "ol.repro.path",
    "ol.snapshot.auto_validate",
    "ol.snapshot.validate.mode",
    "ol.agent.prompt_id"
  )
  old_opts <- stats::setNames(lapply(opt_names, getOption), opt_names)
  on.exit(do.call(options, old_opts), add = TRUE)

  prev <- lake_strict_on(path = tmpdir, prompt_id = "shortcut-prompt")

  expect_true(is.list(prev))
  expect_true(isTRUE(getOption("ol.repro.capture")))
  expect_true(isTRUE(getOption("ol.repro.strict")))
  expect_true(isTRUE(getOption("ol.repro.require_clean_git")))
  expect_equal(getOption("ol.repro.path"), normalizePath(tmpdir, winslash = "/", mustWork = FALSE))
  expect_true(isTRUE(getOption("ol.snapshot.auto_validate")))
  expect_equal(getOption("ol.snapshot.validate.mode"), "error")
  expect_equal(getOption("ol.agent.prompt_id"), "shortcut-prompt")
})
