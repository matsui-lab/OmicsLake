test_that("ol_aggregate calculates basic statistics", {
  ol_init("test_agg_basic")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:100),
    expression = rnorm(100, mean = 50, sd = 10)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result <- ol_aggregate("genes",
                         mean_expr = list(func = "avg", col = "expression"),
                         sd_expr = list(func = "stddev", col = "expression"))
  
  expect_equal(nrow(result), 1)
  expect_true("mean_expr" %in% colnames(result))
  expect_true("sd_expr" %in% colnames(result))
  expect_true(abs(result$mean_expr - 50) < 5)
})

test_that("ol_aggregate works with GROUP BY", {
  ol_init("test_agg_group")
  
  test_data <- data.frame(
    sample = rep(c("A", "B"), each = 50),
    expression = c(rnorm(50, 40, 5), rnorm(50, 60, 5))
  )
  ol_write("data", test_data)
  
  result <- ol_aggregate("data",
                         group_by = "sample",
                         mean_expr = list(func = "avg", col = "expression"),
                         count = list(func = "count", col = "*"))
  
  expect_equal(nrow(result), 2)
  expect_true("sample" %in% colnames(result))
  expect_true("mean_expr" %in% colnames(result))
  expect_true("count" %in% colnames(result))
  expect_equal(result$count, c(50, 50))
})

test_that("ol_aggregate supports multiple aggregate functions", {
  ol_init("test_agg_multi")
  
  test_data <- data.frame(
    category = rep(c("X", "Y"), each = 20),
    value = c(1:20, 21:40)
  )
  ol_write("data", test_data)
  
  result <- ol_aggregate("data",
                         group_by = "category",
                         min_val = list(func = "min", col = "value"),
                         max_val = list(func = "max", col = "value"),
                         avg_val = list(func = "avg", col = "value"),
                         median_val = list(func = "median", col = "value"))
  
  expect_equal(nrow(result), 2)
  expect_true(all(c("min_val", "max_val", "avg_val", "median_val") %in% colnames(result)))
})

test_that("ol_aggregate validates input parameters", {
  ol_init("test_agg_validation")
  
  ol_write("data", data.frame(x = 1:10))
  
  expect_error(
    ol_aggregate(""),
    "table must be a non-empty character string"
  )
  
  expect_error(
    ol_aggregate("data"),
    "At least one aggregate must be specified"
  )
  
  expect_error(
    ol_aggregate("data", list(func = "avg", col = "x")),
    "At least one aggregate must be specified"
  )
  
  expect_error(
    ol_aggregate("data", bad = "not a list"),
    "Each aggregate must be a list"
  )
})

test_that("ol_add_rank adds ranking column", {
  ol_init("test_rank_basic")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:10),
    expression = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result <- ol_add_rank("genes", rank_by = "expression", method = "row_number")
  
  expect_equal(nrow(result), 10)
  expect_true("rank" %in% colnames(result))
  expect_equal(result$rank[result$expression == 95], 1)
  expect_equal(result$rank[result$expression == 5], 10)
})

test_that("ol_add_rank supports different ranking methods", {
  ol_init("test_rank_methods")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:10),
    expression = c(10, 20, 20, 30, 30, 30, 40, 50, 50, 60)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result_row <- ol_add_rank("genes", rank_by = "expression", method = "row_number", as_column = "rn")
  result_rank <- ol_add_rank("genes", rank_by = "expression", method = "rank", as_column = "r")
  result_dense <- ol_add_rank("genes", rank_by = "expression", method = "dense_rank", as_column = "dr")
  
  expect_true("rn" %in% colnames(result_row))
  expect_true("r" %in% colnames(result_rank))
  expect_true("dr" %in% colnames(result_dense))
})

test_that("ol_add_rank works with partitioning", {
  ol_init("test_rank_partition")
  
  test_data <- data.frame(
    sample = rep(c("A", "B"), each = 5),
    gene_id = paste0("G", 1:10),
    expression = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result <- ol_add_rank("genes", 
                        rank_by = "expression", 
                        partition_by = "sample",
                        as_column = "sample_rank")
  
  expect_equal(nrow(result), 10)
  expect_true("sample_rank" %in% colnames(result))
  
  result_a <- result[result$sample == "A", ]
  result_b <- result[result$sample == "B", ]
  expect_equal(max(result_a$sample_rank), 5)
  expect_equal(max(result_b$sample_rank), 5)
})

test_that("ol_add_rank supports ascending order", {
  ol_init("test_rank_asc")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:5),
    expression = c(50, 40, 30, 20, 10)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result <- ol_add_rank("genes", rank_by = "expression", descending = FALSE)
  
  expect_equal(result$rank[result$expression == 10], 1)
  expect_equal(result$rank[result$expression == 50], 5)
})

test_that("ol_add_rank validates input parameters", {
  ol_init("test_rank_validation")
  
  ol_write("data", data.frame(x = 1:10))
  
  expect_error(
    ol_add_rank("", rank_by = "x"),
    "table must be a non-empty character string"
  )
  
  expect_error(
    ol_add_rank("data"),
    "rank_by must be a non-empty character string"
  )
  
  expect_error(
    ol_add_rank("data", rank_by = "x", method = "invalid"),
    "method must be one of"
  )
})

test_that("ol_moving_avg calculates moving average", {
  ol_init("test_ma_basic")
  
  test_data <- data.frame(
    time = 1:10,
    value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  )
  ol_write("data", test_data)
  
  result <- ol_moving_avg("data", "value", window_size = 3, order_by = "time")
  
  expect_equal(nrow(result), 10)
  expect_true("value_ma3" %in% colnames(result))
  expect_true(abs(result$value_ma3[result$time == 5] - 50) < 1)
})

test_that("ol_moving_avg works with different window sizes", {
  ol_init("test_ma_window")
  
  test_data <- data.frame(
    id = 1:20,
    value = rnorm(20, 50, 10)
  )
  ol_write("data", test_data)
  
  result3 <- ol_moving_avg("data", "value", window_size = 3, order_by = "id")
  result5 <- ol_moving_avg("data", "value", window_size = 5, order_by = "id", as_column = "ma5")
  result7 <- ol_moving_avg("data", "value", window_size = 7, order_by = "id", as_column = "ma7")
  
  expect_true("value_ma3" %in% colnames(result3))
  expect_true("ma5" %in% colnames(result5))
  expect_true("ma7" %in% colnames(result7))
})

test_that("ol_moving_avg works with partitioning", {
  ol_init("test_ma_partition")
  
  test_data <- data.frame(
    group = rep(c("A", "B"), each = 10),
    time = rep(1:10, 2),
    value = rnorm(20, 50, 10)
  )
  ol_write("data", test_data)
  
  result <- ol_moving_avg("data", "value", 
                          window_size = 3, 
                          partition_by = "group",
                          order_by = "time")
  
  expect_equal(nrow(result), 20)
  expect_true("value_ma3" %in% colnames(result))
})

test_that("ol_moving_avg validates input parameters", {
  ol_init("test_ma_validation")
  
  ol_write("data", data.frame(x = 1:10, y = 1:10))
  
  expect_error(
    ol_moving_avg("", "x", order_by = "y"),
    "table must be a non-empty character string"
  )
  
  expect_error(
    ol_moving_avg("data", "x"),
    "order_by must be a non-empty character string"
  )
  
  expect_error(
    ol_moving_avg("data", "x", window_size = -1, order_by = "y"),
    "window_size must be a positive integer"
  )
})

test_that("ol_cumulative_sum calculates cumulative sum", {
  ol_init("test_cumsum_basic")
  
  test_data <- data.frame(
    time = 1:10,
    value = rep(10, 10)
  )
  ol_write("data", test_data)
  
  result <- ol_cumulative_sum("data", "value", order_by = "time")
  
  expect_equal(nrow(result), 10)
  expect_true("value_cumsum" %in% colnames(result))
  expect_equal(result$value_cumsum[result$time == 5], 50)
  expect_equal(result$value_cumsum[result$time == 10], 100)
})

test_that("ol_cumulative_sum works with partitioning", {
  ol_init("test_cumsum_partition")
  
  test_data <- data.frame(
    group = rep(c("A", "B"), each = 5),
    time = rep(1:5, 2),
    value = rep(1, 10)
  )
  ol_write("data", test_data)
  
  result <- ol_cumulative_sum("data", "value",
                              partition_by = "group",
                              order_by = "time")
  
  expect_equal(nrow(result), 10)
  expect_true("value_cumsum" %in% colnames(result))
  
  result_a <- result[result$group == "A", ]
  result_b <- result[result$group == "B", ]
  expect_equal(max(result_a$value_cumsum), 5)
  expect_equal(max(result_b$value_cumsum), 5)
})

test_that("ol_cumulative_sum validates input parameters", {
  ol_init("test_cumsum_validation")
  
  ol_write("data", data.frame(x = 1:10, y = 1:10))
  
  expect_error(
    ol_cumulative_sum("", "x", order_by = "y"),
    "table must be a non-empty character string"
  )
  
  expect_error(
    ol_cumulative_sum("data", "x"),
    "order_by must be a non-empty character string"
  )
})

test_that("ol_top_n returns top N rows", {
  ol_init("test_topn_basic")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:100),
    expression = rnorm(100, 50, 10)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result <- ol_top_n("genes", n = 10, order_by = "expression")
  
  expect_equal(nrow(result), 10)
  expect_true(all(result$expression >= quantile(test_data$expression, 0.9)))
})

test_that("ol_top_n works with partitioning", {
  ol_init("test_topn_partition")
  
  test_data <- data.frame(
    sample = rep(c("A", "B", "C"), each = 20),
    gene_id = paste0("G", 1:60),
    expression = rnorm(60, 50, 10)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result <- ol_top_n("genes", n = 5, 
                     order_by = "expression",
                     partition_by = "sample")
  
  expect_equal(nrow(result), 15)
  expect_equal(length(unique(result$sample)), 3)
  expect_equal(sum(result$sample == "A"), 5)
  expect_equal(sum(result$sample == "B"), 5)
  expect_equal(sum(result$sample == "C"), 5)
})

test_that("ol_top_n supports ascending order", {
  ol_init("test_topn_asc")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:20),
    expression = 1:20
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  result <- ol_top_n("genes", n = 5, order_by = "expression", descending = FALSE)
  
  expect_equal(nrow(result), 5)
  expect_true(all(result$expression <= 5))
})

test_that("ol_top_n validates input parameters", {
  ol_init("test_topn_validation")
  
  ol_write("data", data.frame(x = 1:10))
  
  expect_error(
    ol_top_n("", order_by = "x"),
    "table must be a non-empty character string"
  )
  
  expect_error(
    ol_top_n("data"),
    "order_by must be a non-empty character string"
  )
  
  expect_error(
    ol_top_n("data", n = -5, order_by = "x"),
    "n must be a positive integer"
  )
})

test_that("all aggregation functions support lazy evaluation", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")
  
  ol_init("test_agg_lazy")
  
  test_data <- data.frame(
    gene_id = paste0("G", 1:20),
    expression = rnorm(20, 50, 10),
    sample = rep(c("A", "B"), each = 10)
  )
  tryCatch(ol_drop("genes"), error = function(e) NULL)
  ol_write("genes", test_data)
  
  lazy_agg <- ol_aggregate("genes",
                           mean_expr = list(func = "avg", col = "expression"),
                           collect = FALSE)
  expect_s3_class(lazy_agg, "tbl_duckdb_connection")
  
  lazy_rank <- ol_add_rank("genes", rank_by = "expression", collect = FALSE)
  expect_s3_class(lazy_rank, "tbl_duckdb_connection")
  
  lazy_ma <- ol_moving_avg("genes", "expression", order_by = "gene_id", collect = FALSE)
  expect_s3_class(lazy_ma, "tbl_duckdb_connection")
  
  lazy_cumsum <- ol_cumulative_sum("genes", "expression", order_by = "gene_id", collect = FALSE)
  expect_s3_class(lazy_cumsum, "tbl_duckdb_connection")
  
  lazy_topn <- ol_top_n("genes", n = 5, order_by = "expression", collect = FALSE)
  expect_s3_class(lazy_topn, "tbl_duckdb_connection")
})
