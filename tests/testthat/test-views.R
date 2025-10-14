test_that("ol_create_view creates a basic view", {
  ol_init("test_view_basic")
  
  test_data <- data.frame(
    gene_id = paste0("GENE", 1:100),
    expression = rnorm(100, mean = 50, sd = 20),
    stringsAsFactors = FALSE
  )
  ol_write("genes", test_data)
  
  ol_create_view("high_expr", "SELECT * FROM genes WHERE expression > 50")
  
  result <- ol_read("high_expr")
  
  expect_true(nrow(result) > 0)
  expect_true(all(result$expression > 50))
  expect_true("gene_id" %in% colnames(result))
  expect_true("expression" %in% colnames(result))
})

test_that("ol_create_view replaces existing view by default", {
  ol_init("test_view_replace")
  
  ol_write("genes", data.frame(id = 1:10, value = 1:10))
  
  ol_create_view("my_view", "SELECT * FROM genes WHERE value > 5")
  result1 <- ol_read("my_view")
  expect_equal(nrow(result1), 5)
  
  ol_create_view("my_view", "SELECT * FROM genes WHERE value <= 5")
  result2 <- ol_read("my_view")
  expect_equal(nrow(result2), 5)
  expect_true(all(result2$value <= 5))
})

test_that("ol_create_view works with JOINs for version comparison", {
  ol_init("test_view_join")
  
  genes_v1 <- data.frame(
    gene_id = paste0("GENE", 1:50),
    expression = rnorm(50, mean = 100, sd = 20),
    stringsAsFactors = FALSE
  )
  ol_write("genes_v1", genes_v1)
  
  genes_v2 <- data.frame(
    gene_id = paste0("GENE", 1:50),
    expression = genes_v1$expression + rnorm(50, mean = 10, sd = 5),
    stringsAsFactors = FALSE
  )
  ol_write("genes_v2", genes_v2)
  
  ol_create_view("version_comparison",
    "SELECT 
      v1.gene_id,
      v1.expression as expr_v1,
      v2.expression as expr_v2,
      (v2.expression - v1.expression) as change
     FROM genes_v1 v1
     JOIN genes_v2 v2 ON v1.gene_id = v2.gene_id",
    depends_on = c("genes_v1", "genes_v2")
  )
  
  result <- ol_read("version_comparison")
  
  expect_equal(nrow(result), 50)
  expect_true(all(c("gene_id", "expr_v1", "expr_v2", "change") %in% colnames(result)))
  expect_equal(result$change, result$expr_v2 - result$expr_v1)
})

test_that("ol_create_view works with aggregations", {
  ol_init("test_view_agg")
  
  test_data <- data.frame(
    gene_id = rep(paste0("GENE", 1:10), each = 5),
    sample = rep(paste0("S", 1:5), times = 10),
    expression = rnorm(50, mean = 100, sd = 20),
    stringsAsFactors = FALSE
  )
  ol_write("expr_data", test_data)
  
  ol_create_view("gene_summary",
    "SELECT 
      gene_id,
      COUNT(*) as n_samples,
      AVG(expression) as mean_expr,
      STDDEV(expression) as sd_expr,
      MIN(expression) as min_expr,
      MAX(expression) as max_expr
     FROM expr_data
     GROUP BY gene_id"
  )
  
  result <- ol_read("gene_summary")
  
  expect_equal(nrow(result), 10)
  expect_equal(result$n_samples, rep(5, 10))
  expect_true(all(c("gene_id", "mean_expr", "sd_expr", "min_expr", "max_expr") %in% colnames(result)))
})

test_that("ol_create_view tracks dependencies correctly", {
  ol_init("test_view_deps")
  
  ol_write("source1", data.frame(id = 1:10, value = 1:10))
  ol_write("source2", data.frame(id = 1:10, value = 11:20))
  
  ol_create_view("combined",
    "SELECT s1.id, s1.value as val1, s2.value as val2 
     FROM source1 s1 JOIN source2 s2 ON s1.id = s2.id",
    depends_on = c("source1", "source2")
  )
  
  deps <- ol_get_dependencies("combined", direction = "upstream")
  
  expect_true(nrow(deps) >= 2)
  expect_true("source1" %in% deps$parent_name)
  expect_true("source2" %in% deps$parent_name)
})

test_that("ol_list_views returns all views with their definitions", {
  ol_init("test_list_views")
  
  ol_write("data1", data.frame(x = 1:10))
  ol_write("data2", data.frame(x = 11:20))
  
  ol_create_view("view1", "SELECT * FROM data1 WHERE x > 5")
  ol_create_view("view2", "SELECT * FROM data2 WHERE x < 15", depends_on = "data2")
  
  views <- ol_list_views()
  
  expect_true(is.data.frame(views))
  expect_equal(nrow(views), 2)
  expect_true(all(c("view_name", "definition", "dependencies") %in% colnames(views)))
  expect_true("view1" %in% views$view_name)
  expect_true("view2" %in% views$view_name)
  
  view2_row <- views[views$view_name == "view2", ]
  expect_true(grepl("data2", view2_row$dependencies))
})

test_that("ol_list_views returns empty data frame when no views exist", {
  ol_init("test_list_views_empty")
  
  views <- ol_list_views()
  
  expect_true(is.data.frame(views))
  expect_equal(nrow(views), 0)
  expect_true(all(c("view_name", "definition", "dependencies") %in% colnames(views)))
})

test_that("ol_drop_view removes a view", {
  ol_init("test_drop_view")
  
  ol_write("data", data.frame(x = 1:10))
  ol_create_view("my_view", "SELECT * FROM data WHERE x > 5")
  
  views_before <- ol_list_views()
  expect_true("my_view" %in% views_before$view_name)
  
  result <- ol_drop_view("my_view")
  expect_true(result)
  
  views_after <- ol_list_views()
  expect_false("my_view" %in% views_after$view_name)
})

test_that("ol_drop_view removes view dependencies", {
  ol_init("test_drop_view_deps")
  
  ol_write("source", data.frame(x = 1:10))
  ol_create_view("my_view", "SELECT * FROM source", depends_on = "source")
  
  deps_before <- ol_get_dependencies("my_view", direction = "upstream")
  expect_true(nrow(deps_before) > 0)
  
  ol_drop_view("my_view")
  
  deps_after <- ol_get_dependencies("my_view", direction = "upstream")
  expect_equal(nrow(deps_after), 0)
})

test_that("ol_create_view validates input parameters", {
  ol_init("test_view_validation")
  
  ol_write("data", data.frame(x = 1:10))
  
  expect_error(
    ol_create_view("", "SELECT * FROM data"),
    "view name must be a non-empty string"
  )
  
  expect_error(
    ol_create_view("my_view", ""),
    "sql must be a non-empty character string"
  )
  
  expect_error(
    ol_create_view("my_view", NULL),
    "sql must be a non-empty character string"
  )
  
  expect_error(
    ol_create_view("my_view", "SELECT * FROM data", depends_on = 123),
    "depends_on must be a character vector"
  )
})

test_that("ol_create_view fails gracefully with invalid SQL", {
  ol_init("test_view_bad_sql")
  
  expect_error(
    ol_create_view("bad_view", "SELECT * FROM nonexistent_table"),
    "Failed to create view"
  )
})

test_that("complete workflow: compare expression across conditions", {
  ol_init("test_view_workflow")
  
  baseline <- data.frame(
    gene_id = paste0("GENE", 1:100),
    expression = rnorm(100, mean = 100, sd = 30),
    condition = "baseline",
    stringsAsFactors = FALSE
  )
  ol_write("baseline_expr", baseline)
  
  treatment <- data.frame(
    gene_id = paste0("GENE", 1:100),
    expression = baseline$expression + rnorm(100, mean = 20, sd = 15),
    condition = "treatment",
    stringsAsFactors = FALSE
  )
  ol_write("treatment_expr", treatment)
  
  ol_create_view("de_analysis",
    "SELECT 
      b.gene_id,
      b.expression as baseline_expr,
      t.expression as treatment_expr,
      (t.expression - b.expression) as fold_change,
      CASE 
        WHEN (t.expression - b.expression) > 20 THEN 'upregulated'
        WHEN (t.expression - b.expression) < -20 THEN 'downregulated'
        ELSE 'unchanged'
      END as regulation_status
     FROM baseline_expr b
     JOIN treatment_expr t ON b.gene_id = t.gene_id",
    depends_on = c("baseline_expr", "treatment_expr")
  )
  
  result <- ol_read("de_analysis")
  
  expect_equal(nrow(result), 100)
  expect_true(all(c("gene_id", "baseline_expr", "treatment_expr", "fold_change", "regulation_status") %in% colnames(result)))
  
  upregulated <- ol_query("SELECT * FROM de_analysis WHERE regulation_status = 'upregulated'")
  expect_true(all(upregulated$fold_change > 20))
  
  deps <- ol_get_dependencies("de_analysis", direction = "upstream")
  expect_equal(nrow(deps), 2)
})

test_that("views can be queried with ol_query", {
  ol_init("test_view_query")
  
  test_data <- data.frame(
    id = 1:100,
    value = rnorm(100, mean = 50, sd = 20),
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  ol_write("test_table", test_data)
  
  ol_create_view("filtered_view", "SELECT * FROM test_table WHERE value > 50")
  
  result <- ol_query("SELECT category, COUNT(*) as n, AVG(value) as avg_val FROM filtered_view GROUP BY category")
  
  expect_true(is.data.frame(result))
  expect_true("category" %in% colnames(result))
  expect_true("n" %in% colnames(result))
  expect_true("avg_val" %in% colnames(result))
})

test_that("views work with SQL queries", {
  ol_init("test_view_query")
  
  test_data <- data.frame(
    id = 1:100,
    value = rnorm(100),
    stringsAsFactors = FALSE
  )
  ol_write("test_table", test_data)
  
  ol_create_view("test_view", "SELECT * FROM test_table WHERE value > 0")
  
  result <- ol_query("SELECT * FROM test_view")
  
  expect_true(nrow(result) > 0)
  expect_true(all(result$value > 0))
})

test_that("views support window functions", {
  ol_init("test_view_window")
  
  test_data <- data.frame(
    category = rep(c("A", "B"), each = 50),
    value = rnorm(100, mean = 100, sd = 20),
    stringsAsFactors = FALSE
  )
  ol_write("test_data", test_data)
  
  ol_create_view("ranked_view",
    "SELECT 
      category,
      value,
      ROW_NUMBER() OVER (PARTITION BY category ORDER BY value DESC) as rank
     FROM test_data"
  )
  
  result <- ol_read("ranked_view")
  
  expect_equal(nrow(result), 100)
  expect_true("rank" %in% colnames(result))
  
  top_a <- result[result$category == "A" & result$rank == 1, ]
  expect_equal(nrow(top_a), 1)
})

test_that("multiple views can reference each other", {
  ol_init("test_view_chain")
  
  ol_write("base_data", data.frame(id = 1:100, value = rnorm(100)))
  
  ol_create_view("view1", "SELECT * FROM base_data WHERE value > 0")
  ol_create_view("view2", "SELECT * FROM view1 WHERE id > 50")
  
  result <- ol_read("view2")
  
  expect_true(all(result$value > 0))
  expect_true(all(result$id > 50))
})
