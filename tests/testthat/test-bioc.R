test_that("ol_fread reads table with filtering", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    feature = c("gene1", "gene2", "gene3"),
    value = c(10, 20, 30),
    category = c("A", "B", "A")
  )
  
  ol_write("test_table", test_data)
  
  result <- ol_fread("test_table", select = c("feature", "value"))
  expect_equal(ncol(result), 2)
  
  result <- ol_fread("test_table", nrows = 2)
  expect_equal(nrow(result), 2)
})

test_that("ol_read_se creates SummarizedExperiment", {
  skip_if_not_installed("SummarizedExperiment")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    feature = rep(c("gene1", "gene2", "gene3"), each = 2),
    sample = rep(c("sample1", "sample2"), 3),
    value = c(10, 15, 20, 25, 30, 35)
  )
  
  ol_write("test_counts", test_data)
  
  se <- ol_read_se("test_counts", backing = "memory")
  
  expect_s4_class(se, "SummarizedExperiment")
  expect_equal(nrow(se), 3)
  expect_equal(ncol(se), 2)
})

test_that("ol_read_se handles custom column names", {
  skip_if_not_installed("SummarizedExperiment")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    gene_id = rep(c("A", "B"), each = 2),
    sample_id = rep(c("S1", "S2"), 2),
    expression = c(1, 2, 3, 4)
  )
  
  ol_write("custom_table", test_data)
  
  se <- ol_read_se(
    "custom_table",
    feature_col = "gene_id",
    sample_col = "sample_id",
    value_col = "expression",
    backing = "memory"
  )
  
  expect_s4_class(se, "SummarizedExperiment")
  expect_equal(nrow(se), 2)
  expect_equal(ncol(se), 2)
})

test_that("ol_read_mae creates MultiAssayExperiment", {
  skip_if_not_installed("MultiAssayExperiment")
  skip_if_not_installed("SummarizedExperiment")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  rna_data <- data.frame(
    feature = rep(c("gene1", "gene2"), each = 2),
    sample = rep(c("S1", "S2"), 2),
    value = c(10, 20, 30, 40)
  )
  
  protein_data <- data.frame(
    feature = rep(c("prot1", "prot2"), each = 2),
    sample = rep(c("S1", "S2"), 2),
    value = c(5, 15, 25, 35)
  )
  
  ol_write("rna", rna_data)
  ol_write("protein", protein_data)
  
  assays_config <- list(
    rna = list(name = "rna"),
    protein = list(name = "protein")
  )
  
  mae <- ol_read_mae(assays_config, backing = "memory")
  
  expect_s4_class(mae, "MultiAssayExperiment")
  expect_equal(length(mae), 2)
})
