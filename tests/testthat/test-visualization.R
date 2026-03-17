test_that("ol_plot_lineage requires igraph package", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_req")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  
  expect_true(exists("ol_plot_lineage"))
})

test_that("ol_plot_lineage handles linear dependencies", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_linear")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  ol_save("C", list(x = 3), depends_on = "B")
  
  pdf(NULL)
  g <- ol_plot_lineage("C", direction = "upstream")
  dev.off()
  
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 3)
  expect_equal(igraph::ecount(g), 2)
})

test_that("ol_plot_lineage handles branching dependencies", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_branch")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  ol_save("C", list(x = 3), depends_on = "A")
  ol_save("D", list(x = 4), depends_on = c("B", "C"))
  
  pdf(NULL)
  g <- ol_plot_lineage("D", direction = "upstream")
  dev.off()
  
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 4)
  expect_equal(igraph::ecount(g), 4)
})

test_that("ol_plot_lineage handles downstream dependencies", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_down")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  ol_save("C", list(x = 3), depends_on = "A")
  
  pdf(NULL)
  g <- ol_plot_lineage("A", direction = "downstream")
  dev.off()
  
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 3)
  expect_equal(igraph::ecount(g), 2)
})

test_that("ol_plot_lineage handles mixed table and object types", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_mixed")
  
  ol_write("table_A", data.frame(x = 1:3))
  ol_save("object_B", list(y = 4:6))
  ol_write("table_C", data.frame(z = 7:9), depends_on = c("table_A", "object_B"))
  
  pdf(NULL)
  g <- ol_plot_lineage("table_C", direction = "upstream")
  dev.off()
  
  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 3)
  
  expect_true(!is.null(igraph::V(g)$color))
})

test_that("ol_plot_lineage handles empty dependencies", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_empty")
  
  ol_save("A", list(x = 1))
  
  expect_message(
    g <- ol_plot_lineage("A", direction = "upstream"),
    "No dependencies found"
  )
  expect_null(g)
})

test_that("ol_plot_lineage supports different layouts", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_layout")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  ol_save("C", list(x = 3), depends_on = "B")
  
  pdf(NULL)
  
  g1 <- ol_plot_lineage("C", direction = "upstream", layout = "tree")
  expect_s3_class(g1, "igraph")
  
  g2 <- ol_plot_lineage("C", direction = "upstream", layout = "sugiyama")
  expect_s3_class(g2, "igraph")
  
  g3 <- ol_plot_lineage("C", direction = "upstream", layout = "circle")
  expect_s3_class(g3, "igraph")
  
  g4 <- ol_plot_lineage("C", direction = "upstream", layout = "auto")
  expect_s3_class(g4, "igraph")
  
  dev.off()
})

test_that("ol_plot_lineage handles cyclic dependencies", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_cycle")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  
  state <- OmicsLake:::.ol_get_backend_state("test_viz_cycle")
  OmicsLake:::.ol_record_dependency(state, "A", "object", "B", "object")
  
  pdf(NULL)
  expect_no_error(g <- ol_plot_lineage("A", direction = "upstream", max_depth = 3))
  dev.off()
})

test_that("ol_plot_lineage respects max_depth parameter", {
  skip_if_not_installed("igraph")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_viz_depth")
  
  ol_save("A", list(x = 1))
  ol_save("B", list(x = 2), depends_on = "A")
  ol_save("C", list(x = 3), depends_on = "B")
  ol_save("D", list(x = 4), depends_on = "C")
  
  pdf(NULL)
  g <- ol_plot_lineage("D", direction = "upstream", max_depth = 2)
  dev.off()
  
  expect_s3_class(g, "igraph")
  expect_lte(igraph::vcount(g), 3)
})
