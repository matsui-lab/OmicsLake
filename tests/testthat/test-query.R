test_that("ol_query executes basic SELECT query", {
  ol_init("test_query_basic")
  
  test_data <- data.frame(
    gene_id = paste0("GENE", 1:10),
    expression = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95),
    stringsAsFactors = FALSE
  )
  ol_write("genes", test_data)
  
  result <- ol_query("SELECT * FROM genes WHERE expression > 50")
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_true(all(result$expression > 50))
})

test_that("ol_query works with JOIN queries", {
  ol_init("test_query_join")
  
  genes <- data.frame(
    gene_id = paste0("G", 1:5),
    expression = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )
  
  annotations <- data.frame(
    gene_id = paste0("G", c(1, 2, 3)),
    name = c("GeneA", "GeneB", "GeneC"),
    stringsAsFactors = FALSE
  )
  
  ol_write("genes", genes)
  ol_write("annotations", annotations)
  
  result <- ol_query("
    SELECT g.gene_id, g.expression, a.name
    FROM genes g
    JOIN annotations a ON g.gene_id = a.gene_id
  ")
  
  expect_equal(nrow(result), 3)
  expect_true("name" %in% colnames(result))
  expect_true("expression" %in% colnames(result))
})

test_that("ol_query supports aggregation functions", {
  ol_init("test_query_agg")
  
  test_data <- data.frame(
    category = rep(c("A", "B"), each = 5),
    value = c(1:5, 6:10),
    stringsAsFactors = FALSE
  )
  ol_write("data", test_data)
  
  result <- ol_query("
    SELECT category, 
           COUNT(*) as count,
           AVG(value) as avg_value,
           MAX(value) as max_value
    FROM data
    GROUP BY category
  ")
  
  expect_equal(nrow(result), 2)
  expect_equal(result$count, c(5, 5))
  expect_true("avg_value" %in% colnames(result))
})

test_that("ol_query with collect=FALSE returns lazy table", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")
  
  ol_init("test_query_lazy")
  
  test_data <- data.frame(
    id = 1:20,
    value = rnorm(20),
    stringsAsFactors = FALSE
  )
  ol_write("data", test_data)
  
  lazy_tbl <- ol_query("SELECT * FROM data", collect = FALSE)
  
  expect_s3_class(lazy_tbl, "tbl_duckdb_connection")
  
  result <- lazy_tbl %>%
    dplyr::filter(id <= 10) %>%
    dplyr::collect()
  
  expect_equal(nrow(result), 10)
})

test_that("ol_query handles parameterized queries", {
  ol_init("test_query_params")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:10),
    expression = seq(10, 100, by = 10),
    stringsAsFactors = FALSE
  )
  ol_write("genes", test_data)
  
  result <- ol_query(
    "SELECT * FROM genes WHERE expression > :threshold",
    params = list(threshold = 50)
  )
  
  expect_true(all(result$expression > 50))
  expect_equal(nrow(result), 5)
})

test_that("ol_query handles string parameters correctly", {
  ol_init("test_query_string_params")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:5),
    category = c("A", "B", "A", "B", "C"),
    stringsAsFactors = FALSE
  )
  ol_write("genes", test_data)
  
  result <- ol_query(
    "SELECT * FROM genes WHERE category = :cat",
    params = list(cat = "A")
  )
  
  expect_equal(nrow(result), 2)
  expect_true(all(result$category == "A"))
})

test_that("ol_query fails with invalid SQL", {
  ol_init("test_query_invalid")
  
  expect_error(
    ol_query("SELECT * FROM nonexistent_table"),
    "SQL query failed"
  )
})

test_that("ol_query validates input parameters", {
  ol_init("test_query_validation")
  
  expect_error(
    ol_query(""),
    "sql must be a non-empty character string"
  )
  
  expect_error(
    ol_query(123),
    "sql must be a non-empty character string"
  )
  
  expect_error(
    ol_query(c("SELECT", "FROM")),
    "sql must be a non-empty character string"
  )
})

test_that("ol_query validates params format", {
  ol_init("test_query_params_validation")
  
  ol_write("data", data.frame(x = 1:5))
  
  expect_error(
    ol_query("SELECT * FROM data", params = list(1, 2, 3)),
    "params must be a named list"
  )
  
  expect_error(
    ol_query("SELECT * FROM data", params = list(a = 1, "" = 2)),
    "params must be a named list with non-empty names"
  )
})

test_that("ol_query works with window functions", {
  ol_init("test_query_window")
  
  test_data <- data.frame(
    category = rep(c("A", "B"), each = 5),
    value = c(1:5, 6:10),
    stringsAsFactors = FALSE
  )
  ol_write("data", test_data)
  
  result <- ol_query("
    SELECT category, value,
           ROW_NUMBER() OVER (PARTITION BY category ORDER BY value) as row_num,
           AVG(value) OVER (PARTITION BY category) as avg_by_category
    FROM data
  ")
  
  expect_equal(nrow(result), 10)
  expect_true("row_num" %in% colnames(result))
  expect_true("avg_by_category" %in% colnames(result))
})
