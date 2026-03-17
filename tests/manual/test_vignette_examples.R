# Test script to verify all vignette examples work
# Run this manually: Rscript tests/manual/test_vignette_examples.R

# Load package in development mode
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("devtools package required. Install with: install.packages('devtools')")
}
devtools::load_all()

cat("Testing vignette examples...\n\n")

# Test 1: Basic initialization
cat("Test 1: Project initialization... ")
ol_init("vignette_test")
cat("OK\n")

# Test 2: Table operations
cat("Test 2: Table operations... ")
test_data <- data.frame(
  id = 1:10,
  value = rnorm(10)
)
ol_write("test_table", test_data)
loaded <- ol_read("test_table")
stopifnot(nrow(loaded) == 10)
cat("OK\n")

# Test 3: Object operations
cat("Test 3: Object operations... ")
test_obj <- list(a = 1, b = 2)
ol_save("test_obj", test_obj)
loaded_obj <- ol_read_object("test_obj")
stopifnot(loaded_obj$a == 1)
cat("OK\n")

# Test 4: Dependencies
cat("Test 4: Dependency tracking... ")
ol_save("dep_child", list(x = 1), depends_on = "test_obj")
deps <- ol_get_dependencies("dep_child", direction = "upstream")
stopifnot(nrow(deps) > 0)
cat("OK\n")

# Test 5: Versioning
cat("Test 5: Versioning... ")
ol_save("versioned_obj", list(v = 1))
ol_tag_object("versioned_obj", "v1")
Sys.sleep(0.1)
ol_save("versioned_obj", list(v = 2))
ol_tag_object("versioned_obj", "v2")
versions <- ol_list_object_versions("versioned_obj")
stopifnot(nrow(versions) == 2)
cat("OK\n")

# Test 6: Version comparison
cat("Test 6: Version comparison... ")
comparison <- ol_compare_versions("versioned_obj")
stopifnot(nrow(comparison) == 2)
cat("OK\n")

# Test 7: Lineage
cat("Test 7: Lineage tracking... ")
lineage <- ol_show_lineage("dep_child", direction = "upstream")
stopifnot(nrow(lineage) > 0)
cat("OK\n")

# Test 8: Commits
cat("Test 8: Commit system... ")
commit_id <- ol_commit("Test commit", params = list(test = TRUE))
commits <- ol_log_commits(n = 1)
stopifnot(nrow(commits) >= 1)
cat("OK\n")

# Test 9: Labels
cat("Test 9: Labels... ")
ol_label("test_label")
labels <- ol_list_labels()
stopifnot(nrow(labels) >= 1)
cat("OK\n")

# Test 10: Visualization (if igraph available)
cat("Test 10: Visualization... ")
if (requireNamespace("igraph", quietly = TRUE)) {
  g <- ol_plot_lineage("dep_child", direction = "upstream")
  stopifnot(!is.null(g))
  cat("OK\n")
} else {
  cat("SKIPPED (igraph not available)\n")
}

# Test 11: ol_fread
cat("Test 11: ol_fread... ")
fread_result <- ol_fread("test_table", nrows = 5)
stopifnot(nrow(fread_result) == 5)
cat("OK\n")

# Test 12: Multiple versions with tags
cat("Test 12: Multiple version tags... ")
ol_save("multi_version", list(data = 1))
ol_tag_object("multi_version", "baseline")
Sys.sleep(0.1)
ol_save("multi_version", list(data = 2))
ol_tag_object("multi_version", "improved")
tag_comp <- ol_compare_versions("multi_version", versions = c("baseline", "improved"))
stopifnot(nrow(tag_comp) == 2)
cat("OK\n")

# Test 13: Load specific version by tag
cat("Test 13: Load by tag... ")
baseline <- ol_read_object("multi_version", ref = "@tag(baseline)")
stopifnot(baseline$data == 1)
cat("OK\n")

# Test 14: Tables and objects listing
cat("Test 14: List functions... ")
tables <- ol_list_tables()
objects <- ol_list_objects()
stopifnot(nrow(tables) > 0)
stopifnot(nrow(objects) > 0)
cat("OK\n")

# Test 15: Checkout
cat("Test 15: Checkout... ")
ol_write("new_table", data.frame(x = 1:5))
ol_label("checkpoint1")
ol_write("another_table", data.frame(y = 1:10))
ol_checkout("checkpoint1")
tables_after <- ol_list_tables()
cat("OK\n")

cat("\n✅ All vignette examples work correctly!\n")
