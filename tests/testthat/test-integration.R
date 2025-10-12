test_that("End-to-end workflow: init -> write -> read", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  
  ol_init("integration_test")
  
  counts_data <- data.frame(
    feature = c("gene1", "gene2", "gene3"),
    sample = c("S1", "S1", "S1"),
    value = c(100, 200, 300)
  )
  ol_write("counts", counts_data)
  
  result <- ol_read("counts")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("feature", "sample", "value") %in% names(result)))
})

test_that("Versioning workflow: commit -> label -> read with ref", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  
  ol_init("version_test")
  
  v1_data <- data.frame(x = 1:5, y = letters[1:5])
  ol_write("data", v1_data)
  ol_commit("version 1")
  ol_label("v1")
  
  v2_data <- data.frame(x = 6:10, y = letters[6:10])
  ol_write("data", v2_data, mode = "overwrite")
  ol_commit("version 2")
  
  latest <- ol_read("data", ref = "@latest")
  expect_equal(nrow(latest), 5)
  
  v1_read <- ol_read("data", ref = "@v1")
  expect_s3_class(v1_read, "data.frame")
})

test_that("Object storage workflow: save -> read_object", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  
  ol_init("object_test")
  
  test_list <- list(a = 1:10, b = "test", c = matrix(1:9, 3, 3))
  ol_save("my_list", test_list)
  
  test_model <- lm(mpg ~ wt, data = mtcars)
  ol_save("my_model", test_model)
  
  list_result <- ol_read_object("my_list")
  expect_type(list_result, "list")
  expect_equal(list_result$a, 1:10)
  
  model_result <- ol_read_object("my_model")
  expect_s3_class(model_result, "lm")
})

test_that("Bioconductor workflow: write -> read_se -> mae", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("MultiAssayExperiment")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  
  ol_init("bioc_test")
  
  expression_data <- expand.grid(
    feature = paste0("gene", 1:10),
    sample = paste0("sample", 1:5)
  )
  expression_data$value <- rnorm(nrow(expression_data), mean = 100, sd = 20)
  
  ol_write("expression", expression_data)
  ol_commit("raw expression data")
  ol_label("raw")
  
  se <- ol_read_se("expression", ref = "@raw", backing = "memory")
  
  expect_s4_class(se, "SummarizedExperiment")
  expect_equal(nrow(se), 10)
  expect_equal(ncol(se), 5)
  
  proteomics_data <- expand.grid(
    feature = paste0("prot", 1:5),
    sample = paste0("sample", 1:5)
  )
  proteomics_data$value <- rnorm(nrow(proteomics_data), mean = 50, sd = 10)
  
  ol_write("proteomics", proteomics_data)
  
  assays_config <- list(
    rna = list(name = "expression"),
    protein = list(name = "proteomics")
  )
  
  mae <- ol_read_mae(assays_config, backing = "memory")
  
  expect_s4_class(mae, "MultiAssayExperiment")
  expect_equal(length(mae), 2)
})

test_that("Multiple projects can coexist", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  old_proj <- getOption("ol.project")
  on.exit({
    options(ol.root = old_opt)
    options(ol.project = old_proj)
  }, add = TRUE)
  
  options(ol.root = tmpdir)
  
  ol_init("project_a")
  ol_write("data", data.frame(x = 1:3))
  
  ol_init("project_b")
  ol_write("data", data.frame(y = 4:6))
  
  data_a <- ol_read("data", project = "project_a")
  data_b <- ol_read("data", project = "project_b")
  
  expect_true("x" %in% names(data_a))
  expect_true("y" %in% names(data_b))
  expect_false("y" %in% names(data_a))
  expect_false("x" %in% names(data_b))
})
