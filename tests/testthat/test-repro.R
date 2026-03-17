test_that("ol_commit automatically stores reproducibility context", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)
  withr::local_options(list(
    ol.repro.capture = TRUE,
    ol.repro.path = tmpdir,
    ol.repro.include = c("session", "system")
  ))

  options(ol.root = tmpdir)
  ol_init("proj_repro_auto")

  ol_commit("auto context")
  history <- ol_log_commits(n = 1)
  parsed <- jsonlite::fromJSON(history$params_json[[1]], simplifyVector = FALSE)

  expect_true(is.list(parsed$omicslake_repro))
  expect_true(is.character(parsed$omicslake_repro$session$r_version))
  expect_true(is.character(parsed$omicslake_repro$system$sysname))
})

test_that("ol_commit captures git and renv metadata from reproducibility path", {
  skip_if(Sys.which("git") == "", "git command is unavailable")

  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)

  repo_dir <- file.path(tmpdir, "analysis_repo")
  dir.create(repo_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "{",
      '  "R": {"Version": "4.3.0"},',
      '  "Packages": {"dplyr": {"Version": "1.1.4"}}',
      "}"
    ),
    con = file.path(repo_dir, "renv.lock")
  )
  writeLines("omicslake repro integration", con = file.path(repo_dir, "README.md"))

  run_git <- function(args) {
    out <- suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    list(status = as.integer(status), output = out)
  }

  init_res <- run_git(c("init", repo_dir))
  skip_if(init_res$status != 0L, "git init failed in test environment")
  conf1 <- run_git(c("-C", repo_dir, "config", "user.email", "test@example.com"))
  skip_if(conf1$status != 0L, "git config user.email failed in test environment")
  conf2 <- run_git(c("-C", repo_dir, "config", "user.name", "OmicsLake Test"))
  skip_if(conf2$status != 0L, "git config user.name failed in test environment")
  add_res <- run_git(c("-C", repo_dir, "add", "README.md", "renv.lock"))
  skip_if(add_res$status != 0L, "git add failed in test environment")
  commit_res <- run_git(c("-C", repo_dir, "commit", "-m", "init"))
  skip_if(commit_res$status != 0L, "git commit failed in test environment")

  withr::local_options(list(
    ol.repro.capture = TRUE,
    ol.repro.path = repo_dir,
    ol.repro.include = c("git", "renv")
  ))
  options(ol.root = tmpdir)
  ol_init("proj_repro_git_renv")

  ol_commit("git+renv context")
  history <- ol_log_commits(n = 1)

  expect_true(nzchar(history$git_commit[[1]]))
  expect_true(nzchar(history$git_branch[[1]]))
  expect_false(is.na(history$git_dirty[[1]]))
  expect_true(grepl("renv.lock$", history$renv_lockfile[[1]]))
  expect_true(nzchar(history$renv_lockfile_md5[[1]]))
})

test_that("lake_status and lake_doctor include reproducibility integration signals", {
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

  analysis_dir <- file.path(tmpdir, "analysis")
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("{}", con = file.path(analysis_dir, "renv.lock"))

  withr::local_options(list(
    ol.repro.capture = TRUE,
    ol.repro.path = analysis_dir,
    ol.repro.include = c("git", "renv")
  ))
  options(ol.root = tmpdir)

  use_lake("proj_repro_status")
  put("tbl", data.frame(x = 1:3))

  st <- lake_status()
  expect_true(all(c(
    "repro_path", "git_repo", "git_branch", "git_commit",
    "git_dirty", "renv_lockfile", "renv_lockfile_md5"
  ) %in% names(st)))
  expect_equal(st$repro_path[[1]], normalizePath(analysis_dir, winslash = "/", mustWork = FALSE))
  expect_true(grepl("renv.lock$", st$renv_lockfile[[1]]))

  rep <- lake_doctor(verbose = FALSE)
  expect_true(any(rep$check == "automatic reproducibility metadata capture"))
  expect_true(any(rep$check == "git repository detected"))
  expect_true(any(rep$check == "renv lockfile detected"))
})

test_that("clean git guard can reject commit on dirty working tree", {
  skip_if(Sys.which("git") == "", "git command is unavailable")

  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)

  repo_dir <- file.path(tmpdir, "dirty_repo")
  dir.create(repo_dir, recursive = TRUE, showWarnings = FALSE)
  target_file <- file.path(repo_dir, "tracked.txt")
  writeLines("v1", con = target_file)

  run_git <- function(args) {
    out <- suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    as.integer(status)
  }

  skip_if(run_git(c("init", repo_dir)) != 0L, "git init failed")
  skip_if(run_git(c("-C", repo_dir, "config", "user.email", "test@example.com")) != 0L, "git config user.email failed")
  skip_if(run_git(c("-C", repo_dir, "config", "user.name", "OmicsLake Test")) != 0L, "git config user.name failed")
  skip_if(run_git(c("-C", repo_dir, "add", "tracked.txt")) != 0L, "git add failed")
  skip_if(run_git(c("-C", repo_dir, "commit", "-m", "init")) != 0L, "git commit failed")

  # Make working tree dirty after initial commit
  writeLines(c("v1", "dirty"), con = target_file)

  withr::local_options(list(
    ol.repro.capture = TRUE,
    ol.repro.path = repo_dir,
    ol.repro.include = c("git"),
    ol.repro.require_clean_git = TRUE
  ))
  options(ol.root = tmpdir)
  ol_init("proj_dirty_guard")

  expect_error(
    ol_commit("should fail when dirty"),
    "Git working tree is dirty"
  )
})

test_that("track_script records AI prompt id and script hash in commit metadata", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)

  script_path <- file.path(tmpdir, "analysis_pipeline.R")
  writeLines(
    c(
      "x <- data.frame(v = 1:3)",
      "nrow(x)"
    ),
    con = script_path
  )

  withr::local_options(list(
    ol.repro.capture = TRUE,
    ol.repro.path = tmpdir,
    ol.repro.include = c("session"),
    ol.agent.prompt_id = "prompt-2026-02-21-test"
  ))
  options(ol.root = tmpdir)

  track_script(
    path = script_path,
    project = "proj_agent_metadata",
    snapshot = "script_run_1"
  )

  history <- ol_log_commits(project = "proj_agent_metadata", n = 1)
  expect_equal(history$agent_prompt_id[[1]], "prompt-2026-02-21-test")
  expect_equal(history$agent_script_path[[1]], normalizePath(script_path, winslash = "/", mustWork = TRUE))
  expect_true(nzchar(history$agent_script_md5[[1]]))
})

test_that("snapshot validation compares with previous snapshot and can block structural drift", {
  tmpdir <- withr::local_tempdir()
  old_root <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_root)
    options(ol.project = old_proj)
  }, add = TRUE)

  withr::local_options(list(
    ol.repro.capture = FALSE,
    ol.snapshot.auto_validate = TRUE,
    ol.snapshot.validate.mode = "off"
  ))
  options(ol.root = tmpdir)

  lake <- Lake$new("proj_snapshot_validation")
  lake$put("counts", data.frame(x = 1:3))
  lake$snap("v1")

  lake$put("counts", data.frame(x = 1:4))
  lake$snap("v2")

  history <- ol_log_commits(project = "proj_snapshot_validation", n = 1)
  expect_equal(history$snapshot_validation_prev[[1]], "v1")
  expect_true(history$snapshot_validation_row_count_changes[[1]] >= 1)
  expect_equal(history$snapshot_validation_structural_changes[[1]], 0L)

  withr::local_options(list(ol.snapshot.validate.mode = "error"))
  lake$put("new_table", data.frame(y = 1:2))
  expect_error(
    lake$snap("v3"),
    "Snapshot validation"
  )
})

test_that("ol_enable_strict_repro_mode sets one-command strict defaults", {
  tmpdir <- withr::local_tempdir()
  opt_names <- c(
    "ol.repro.capture",
    "ol.repro.strict",
    "ol.repro.require_clean_git",
    "ol.repro.path",
    "ol.repro.include",
    "ol.snapshot.auto_validate",
    "ol.snapshot.validate.mode",
    "ol.snapshot.validate.max_tables",
    "ol.agent.prompt_id",
    "ol.agent.run_id",
    "ol.agent.name"
  )
  old_opts <- stats::setNames(lapply(opt_names, getOption), opt_names)
  on.exit(do.call(options, old_opts), add = TRUE)

  prev <- ol_enable_strict_repro_mode(
    path = tmpdir,
    prompt_id = "prompt-one-command",
    run_id = "run-one-command",
    agent_name = "codex"
  )

  expect_true(is.list(prev))
  expect_true(isTRUE(getOption("ol.repro.capture")))
  expect_true(isTRUE(getOption("ol.repro.strict")))
  expect_true(isTRUE(getOption("ol.repro.require_clean_git")))
  expect_equal(getOption("ol.repro.path"), normalizePath(tmpdir, winslash = "/", mustWork = FALSE))
  expect_equal(getOption("ol.repro.include"), c("git", "renv", "session", "system"))
  expect_true(isTRUE(getOption("ol.snapshot.auto_validate")))
  expect_equal(getOption("ol.snapshot.validate.mode"), "error")
  expect_equal(getOption("ol.snapshot.validate.max_tables"), 200L)
  expect_equal(getOption("ol.agent.prompt_id"), "prompt-one-command")
  expect_equal(getOption("ol.agent.run_id"), "run-one-command")
  expect_equal(getOption("ol.agent.name"), "codex")
})
