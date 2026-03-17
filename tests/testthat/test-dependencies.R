test_that("ol_save records dependencies correctly", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_deps")
  
  ol_save("raw_data", list(x = 1:10))
  ol_save("processed", list(y = 2:11), depends_on = "raw_data")
  
  deps <- ol_get_dependencies("processed", direction = "upstream")
  
  expect_equal(nrow(deps), 1)
  expect_equal(deps$parent_name[1], "raw_data")
  expect_equal(deps$parent_type[1], "object")
})

test_that("ol_write records dependencies correctly", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_table_deps")
  
  ol_write("raw_counts", data.frame(gene = paste0("G", 1:5), count = 10:14))
  ol_save("norm_factors", list(factors = c(1.1, 0.9, 1.0, 1.2, 0.8)))
  ol_write("normalized_counts", data.frame(gene = paste0("G", 1:5), norm_count = 11:15), 
           depends_on = c("raw_counts", "norm_factors"))
  
  deps <- ol_get_dependencies("normalized_counts", direction = "upstream")
  
  expect_equal(nrow(deps), 2)
  expect_true("raw_counts" %in% deps$parent_name)
  expect_true("norm_factors" %in% deps$parent_name)
})

test_that("ol_get_dependencies returns downstream dependencies", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_downstream")
  
  ol_save("raw", list(data = 1:10))
  ol_save("processed1", list(data = 2:11), depends_on = "raw")
  ol_save("processed2", list(data = 3:12), depends_on = "raw")
  
  deps <- ol_get_dependencies("raw", direction = "downstream")
  
  expect_equal(nrow(deps), 2)
  expect_true("processed1" %in% deps$child_name)
  expect_true("processed2" %in% deps$child_name)
})

test_that("ol_get_dependencies handles both directions", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_both")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  ol_save("C", list(x = 3), depends_on = "B")
  
  deps <- ol_get_dependencies("B", direction = "both")
  
  expect_equal(nrow(deps), 2)
  expect_true("A" %in% deps$name)
  expect_true("C" %in% deps$name)
  expect_true("upstream" %in% deps$direction)
  expect_true("downstream" %in% deps$direction)
})

test_that("ol_show_lineage traces full upstream lineage", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_lineage")
  
  ol_save("raw", list(x = 1))
  ol_save("cleaned", list(x = 2), depends_on = "raw")
  ol_save("normalized", list(x = 3), depends_on = "cleaned")
  ol_save("final", list(x = 4), depends_on = "normalized")
  
  lineage <- ol_show_lineage("final", direction = "upstream")
  
  expect_true(nrow(lineage) >= 3)
  expect_true("raw" %in% lineage$parent)
  expect_true("cleaned" %in% lineage$parent)
  expect_true("normalized" %in% lineage$parent)
})

test_that("ol_show_lineage traces full downstream lineage", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_lineage_down")
  
  ol_save("source", list(x = 1))
  ol_save("derived1", list(x = 2), depends_on = "source")
  ol_save("derived2", list(x = 3), depends_on = "source")
  ol_save("final1", list(x = 4), depends_on = "derived1")
  
  lineage <- ol_show_lineage("source", direction = "downstream")
  
  expect_true(nrow(lineage) >= 3)
  expect_true("derived1" %in% lineage$child)
  expect_true("derived2" %in% lineage$child)
  expect_true("final1" %in% lineage$child)
})

test_that("ol_show_lineage handles cyclic dependencies gracefully", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_cycle")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  
  state <- OmicsLake:::.ol_get_backend_state("test_cycle")
  OmicsLake:::.ol_record_dependency(state, "A", "object", "B", "object")
  
  lineage_up <- ol_show_lineage("A", direction = "upstream", max_depth = 5)
  lineage_down <- ol_show_lineage("A", direction = "downstream", max_depth = 5)
  
  expect_true(is.data.frame(lineage_up))
  expect_true(is.data.frame(lineage_down))
})

test_that("dependencies work with mixed tables and objects", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_mixed")
  
  ol_write("raw_table", data.frame(id = 1:3, value = c(10, 20, 30)))
  ol_save("params", list(threshold = 15))
  ol_write("filtered_table", data.frame(id = 2:3, value = c(20, 30)), 
           depends_on = c("raw_table", "params"))
  
  deps <- ol_get_dependencies("filtered_table", direction = "upstream")
  
  expect_equal(nrow(deps), 2)
  expect_true(any(deps$parent_type == "table"))
  expect_true(any(deps$parent_type == "object"))
})

test_that("dependencies table is empty for items without dependencies", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_no_deps")
  
  ol_save("independent", list(x = 1))
  
  deps <- ol_get_dependencies("independent", direction = "upstream")
  
  expect_equal(nrow(deps), 0)
})
