test_that("ol_list_object_versions lists all versions correctly", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_versions")
  
  ol_save("results", list(data = 1:10))
  Sys.sleep(0.1)
  ol_save("results", list(data = 1:20))
  Sys.sleep(0.1)
  ol_save("results", list(data = 1:30))
  
  versions <- ol_list_object_versions("results")
  
  expect_equal(nrow(versions), 3)
  expect_true(all(c("version_ts", "tags", "size_bytes", "dependencies") %in% names(versions)))
  expect_true(versions$version_ts[1] > versions$version_ts[3])
})

test_that("ol_list_object_versions shows tags correctly", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_versions_tags")
  
  ol_save("results", list(data = 1:10))
  ol_tag_object("results", "v1.0")
  Sys.sleep(0.1)
  ol_save("results", list(data = 1:20))
  ol_tag_object("results", "v2.0")
  
  versions <- ol_list_object_versions("results")
  
  expect_equal(nrow(versions), 2)
  expect_true(grepl("v2.0", versions$tags[1]))
  expect_true(grepl("v1.0", versions$tags[2]))
})

test_that("ol_list_object_versions shows dependencies correctly", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_versions_deps")
  
  ol_save("raw", list(x = 1:10))
  Sys.sleep(0.1)
  ol_save("processed", list(x = 2:11), depends_on = "raw")
  Sys.sleep(0.1)
  ol_save("params", list(alpha = 0.05))
  Sys.sleep(0.1)
  ol_save("processed", list(x = 3:12), depends_on = c("raw", "params"))
  
  versions <- ol_list_object_versions("processed")
  
  expect_equal(nrow(versions), 2)
  expect_true(grepl("raw", versions$dependencies[1]))
  expect_true(grepl("params", versions$dependencies[1]))
  expect_true(grepl("raw", versions$dependencies[2]))
  expect_false(grepl("params", versions$dependencies[2]))
})

test_that("ol_list_object_versions handles empty results", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_empty")
  
  versions <- ol_list_object_versions("nonexistent")
  
  expect_equal(nrow(versions), 0)
  expect_true(all(c("version_ts", "tags", "size_bytes", "dependencies") %in% names(versions)))
})

test_that("ol_compare_versions compares all versions", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_compare")
  
  ol_save("results", list(data = 1:10))
  Sys.sleep(0.1)
  ol_save("results", list(data = 1:20))
  Sys.sleep(0.1)
  ol_save("results", list(data = 1:30))
  
  comparison <- ol_compare_versions("results")
  
  expect_equal(nrow(comparison), 3)
  expect_true("size_change" %in% names(comparison))
  expect_true("time_since_previous" %in% names(comparison))
  expect_true(is.na(comparison$size_change[1]))
  expect_false(is.na(comparison$size_change[2]))
})

test_that("ol_compare_versions shows dependency changes", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_compare_deps")
  
  ol_save("raw", list(x = 1:10))
  ol_save("params1", list(alpha = 0.05))
  Sys.sleep(0.1)
  ol_save("results", list(y = 1:10), depends_on = "raw")
  Sys.sleep(0.1)
  ol_save("params2", list(alpha = 0.01))
  Sys.sleep(0.1)
  ol_save("results", list(y = 1:20), depends_on = c("raw", "params2"))
  
  comparison <- ol_compare_versions("results")
  
  expect_equal(nrow(comparison), 2)
  expect_true("deps_added" %in% names(comparison))
  expect_true("deps_removed" %in% names(comparison))
  expect_true(grepl("params2", comparison$deps_added[1]))
})

test_that("ol_compare_versions works with tagged versions", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_compare_tags")
  
  ol_save("results", list(data = 1:10))
  ol_tag_object("results", "baseline")
  Sys.sleep(0.1)
  ol_save("results", list(data = 1:20))
  ol_tag_object("results", "improved")
  Sys.sleep(0.1)
  ol_save("results", list(data = 1:30))
  ol_tag_object("results", "final")
  
  comparison <- ol_compare_versions("results", versions = c("baseline", "final"))
  
  expect_equal(nrow(comparison), 2)
  expect_true(grepl("baseline", comparison$tags[2]))
  expect_true(grepl("final", comparison$tags[1]))
})

test_that("differential expression workflow use case", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_de_workflow")
  
  ol_write("raw_counts", data.frame(gene = paste0("G", 1:100), count = rpois(100, 100)))
  ol_save("norm_factors", list(factors = runif(100, 0.8, 1.2)))
  Sys.sleep(0.1)
  
  ol_write("normalized_counts", data.frame(gene = paste0("G", 1:100), norm = rpois(100, 100)),
           depends_on = c("raw_counts", "norm_factors"))
  ol_save("de_params_v1", list(method = "DESeq2", alpha = 0.05))
  Sys.sleep(0.1)
  ol_save("de_results", list(genes = paste0("G", 1:10), pval = runif(10, 0, 0.05)),
          depends_on = c("normalized_counts", "de_params_v1"))
  ol_tag_object("de_results", "method_deseq2_alpha0.05")
  Sys.sleep(0.1)
  
  ol_save("de_params_v2", list(method = "DESeq2", alpha = 0.01))
  Sys.sleep(0.1)
  ol_save("de_results", list(genes = paste0("G", 1:5), pval = runif(5, 0, 0.01)),
          depends_on = c("normalized_counts", "de_params_v2"))
  ol_tag_object("de_results", "method_deseq2_alpha0.01")
  Sys.sleep(0.1)
  
  ol_save("de_params_v3", list(method = "edgeR", alpha = 0.05))
  Sys.sleep(0.1)
  ol_save("de_results", list(genes = paste0("G", 1:8), pval = runif(8, 0, 0.05)),
          depends_on = c("normalized_counts", "de_params_v3"))
  ol_tag_object("de_results", "method_edger_alpha0.05")
  
  versions <- ol_list_object_versions("de_results")
  expect_equal(nrow(versions), 3)
  expect_true(all(grepl("normalized_counts", versions$dependencies)))
  
  comparison <- ol_compare_versions("de_results")
  expect_equal(nrow(comparison), 3)
  expect_true(all(c("deps_added", "deps_removed") %in% names(comparison)))
  
  method_comparison <- ol_compare_versions("de_results", 
                                           versions = c("method_deseq2_alpha0.05", "method_edger_alpha0.05"))
  expect_equal(nrow(method_comparison), 2)
})
