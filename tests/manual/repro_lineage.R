# OmicsLake v2.0 Audit Verification Script
# Purpose: Verify lazy/collect, version-aware lineage, and atomicity behavior

library(OmicsLake)

cat("=" %s+% strrep("=", 60), "\n")
cat("OmicsLake v2.0 DataLineage Audit Verification\n")
cat("=" %s+% strrep("=", 60), "\n\n")

# Helper
`%s+%` <- function(a, b) paste0(a, b)

cleanup <- function(project) {
  unlink(file.path(path.expand("~"), ".omicslake", project), recursive = TRUE)
}

# =============================================================================
# Test 1: Lazy/Collect Semantics
# =============================================================================
cat("## Test 1: Lazy/Collect Semantics\n")
cat("-" %s+% strrep("-", 40), "\n")

lake <- Lake$new("audit_lazy_test")
lake$put("t", data.frame(x = 1:10, y = runif(10)))

# Test 1a: get(collect=FALSE) - should return lazy tbl
cat("\n### 1a: lake$get(collect=FALSE)\n")
q <- lake$get("t", collect = FALSE)
cat("Class of result:", paste(class(q), collapse = ", "), "\n")
cat("Is tbl_lazy?:", inherits(q, "tbl_lazy"), "\n")
cat("Is tbl_sql?:", inherits(q, "tbl_sql"), "\n")
cat("Is data.frame (eager)?:", is.data.frame(q), "\n")

# Try show_query
cat("\nTrying dplyr::show_query():\n")
tryCatch({
  print(dplyr::show_query(q))
}, error = function(e) {
  cat("ERROR: Cannot show query -", conditionMessage(e), "\n")
  cat("VERDICT: Data was collected eagerly!\n")
})

# Test 1b: get with where/select + collect=FALSE
cat("\n### 1b: lake$get(where=~x>5, select='x', collect=FALSE)\n")
q2 <- tryCatch({
  lake$get("t", where = ~x > 5, select = c("x"), collect = FALSE)
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(q2)) {
  cat("Class:", paste(class(q2), collapse = ", "), "\n")
  cat("Is data.frame (eager)?:", is.data.frame(q2), "\n")
  tryCatch({
    dplyr::show_query(q2)
    cat("SQL pushdown works!\n")
  }, error = function(e) {
    cat("No SQL query - filter/select applied in R after collect\n")
  })
}

# Test 1c: lake$ref() behavior
cat("\n### 1c: lake$ref() lazy behavior\n")
lazy_ref <- lake$ref("t")
cat("Class:", paste(class(lazy_ref), collapse = ", "), "\n")
cat("Has lake_source attr:", !is.null(attr(lazy_ref, "lake_source")), "\n")
tryCatch({
  print(dplyr::show_query(lazy_ref))
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

cleanup("audit_lazy_test")

# =============================================================================
# Test 2: Version-Aware Lineage
# =============================================================================
cat("\n\n## Test 2: Version-Aware Lineage\n")
cat("-" %s+% strrep("-", 40), "\n")

lake2 <- Lake$new("audit_version_test")

# Create versioned data
df1 <- data.frame(x = 1:5, stage = "raw")
df2 <- data.frame(x = 1:5 * 2, stage = "normalized")

lake2$put("a", df1)
lake2$tag("a", "raw")

lake2$put("a", df2)
lake2$tag("a", "norm")

# Check __ol_dependencies schema
cat("\n### 2a: Dependencies table schema\n")
state <- lake2$.__enclos_env__$private$.state
conn <- state$conn
ident <- OmicsLake:::.ol_sql_ident(conn, state, "__ol_dependencies")
cols <- tryCatch({
  query <- sprintf("DESCRIBE %s", ident)
  DBI::dbGetQuery(conn, query)
}, error = function(e) {
  query <- sprintf("SELECT * FROM %s LIMIT 0", ident)
  res <- DBI::dbGetQuery(conn, query)
  data.frame(column_name = names(res))
})
cat("Columns in __ol_dependencies:\n")
print(cols)

cat("\nChecking for version columns:\n")
col_names <- if ("column_name" %in% names(cols)) cols$column_name else names(cols)
has_version_col <- any(grepl("version|ref|commit", col_names, ignore.case = TRUE))
cat("Has parent_version_id:", "parent_version_id" %in% col_names, "\n")
cat("Has parent_ref:", "parent_ref" %in% col_names, "\n")
cat("Has parent_commit_id:", "parent_commit_id" %in% col_names, "\n")
cat("VERDICT: Version-aware columns present:", has_version_col, "\n")

# Test 2b: Dependency tracking preserves ref
cat("\n### 2b: Does dependency tracking preserve ref?\n")

# Use with_tracking to create B from A@tag(raw)
result <- with_tracking(lake2, "b", {
  data <- lake2$get("a", ref = "@tag(raw)")
  data$processed <- TRUE
  data
})

deps_b <- lake2$tree("b", direction = "up")
cat("\nDependencies of 'b':\n")
print(deps_b)

# Check if we can distinguish which version of 'a' was used
cat("\nCan we tell that 'raw' version was used? Check parent columns:\n")
print(names(deps_b))

cleanup("audit_version_test")

# =============================================================================
# Test 3: DependencyTracker Scope
# =============================================================================
cat("\n\n## Test 3: Auto-Tracking Scope\n")
cat("-" %s+% strrep("-", 40), "\n")

lake3 <- Lake$new("audit_scope_test")
lake3$put("source", data.frame(x = 1:5))

# Test 3a: Does simple put() auto-track?
cat("\n### 3a: Simple read-then-put (no wrapper)\n")
data_read <- lake3$get("source")
lake3$put("derived_simple", data_read)

deps_simple <- lake3$deps("derived_simple", "up")
cat("Dependencies detected:", nrow(deps_simple), "\n")
cat("Expected 'source' as parent?:", "source" %in% deps_simple$parent_name, "\n")

# Test 3b: with_tracking
cat("\n### 3b: Using with_tracking()\n")
result <- with_tracking(lake3, "derived_tracked", {
  d <- lake3$get("source")
  d$y <- d$x * 2
  d
})

deps_tracked <- lake3$deps("derived_tracked", "up")
cat("Dependencies detected:", nrow(deps_tracked), "\n")
cat("'source' as parent?:", "source" %in% deps_tracked$parent_name, "\n")

# Test 3c: save_as with dplyr pipe
cat("\n### 3c: Using lake$ref() |> dplyr |> save_as()\n")
lake3$ref("source") |>
  dplyr::mutate(z = x * 3) |>
  save_as("derived_pipe", lake3)

deps_pipe <- lake3$deps("derived_pipe", "up")
cat("Dependencies detected:", nrow(deps_pipe), "\n")
cat("'source' as parent?:", "source" %in% deps_pipe$parent_name, "\n")

cleanup("audit_scope_test")

# =============================================================================
# Test 4: API Bugs
# =============================================================================
cat("\n\n## Test 4: API Inconsistencies\n")
cat("-" %s+% strrep("-", 40), "\n")

lake4 <- Lake$new("audit_api_test")
df <- data.frame(x = 1:5, y = letters[1:5])
lake4$put("data", df)
lake4$tag("data", "v1")
lake4$put("data", data.frame(x = 10:15, y = letters[1:6]))
lake4$tag("data", "v2")

# Test 4a: diff() ref parameters
cat("\n### 4a: diff(name, ref1, ref2) - are refs used?\n")
diff_result <- tryCatch({
  lake4$diff("data", ref1 = "@tag(v1)", ref2 = "@tag(v2)")
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(diff_result)) {
  cat("Result columns:", paste(names(diff_result), collapse = ", "), "\n")
  cat("Check if comparison uses v1 vs v2...\n")
  print(diff_result)
}

# Test 4b: .is_lazy() check
cat("\n### 4b: .is_lazy() implementation check\n")
lazy_tbl <- lake4$ref("data")
cat("lake$ref() class:", paste(class(lazy_tbl), collapse = ", "), "\n")

# Check private method
is_lazy_fn <- lake4$.__enclos_env__$private$.is_lazy
cat("Returns TRUE for tbl_lazy?:", is_lazy_fn(lazy_tbl), "\n")

cleanup("audit_api_test")

# =============================================================================
# Summary
# =============================================================================
cat("\n\n")
cat("=" %s+% strrep("=", 60), "\n")
cat("AUDIT VERIFICATION COMPLETE\n")
cat("=" %s+% strrep("=", 60), "\n")
cat("\nReview output above for PASS/FAIL status of each test.\n")
