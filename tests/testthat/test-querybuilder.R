# Tests for QueryBuilder

test_that("QueryBuilder can be created", {
  lake <- Lake$new("test_qb_init")
  qb <- lake$query()
  expect_s3_class(qb, "QueryBuilder")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_init"), recursive = TRUE)
})

test_that("QueryBuilder from() sets source table", {
  lake <- Lake$new("test_qb_from")

  lake$put("users", data.frame(
    id = 1:5,
    name = c("Alice", "Bob", "Carol", "Dave", "Eve"),
    age = c(25, 30, 35, 40, 45),
    stringsAsFactors = FALSE
  ))

  result <- lake$from("users")$run()
  expect_equal(nrow(result), 5)
  expect_true("name" %in% names(result))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_from"), recursive = TRUE)
})

test_that("QueryBuilder where() filters data", {
  lake <- Lake$new("test_qb_where")

  lake$put("data", data.frame(
    x = 1:10,
    y = letters[1:10],
    stringsAsFactors = FALSE
  ))

  result <- lake$from("data")$
    where(x > 5)$
    run()

  expect_equal(nrow(result), 5)
  expect_true(all(result$x > 5))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_where"), recursive = TRUE)
})

test_that("QueryBuilder select() picks columns", {
  lake <- Lake$new("test_qb_select")

  lake$put("data", data.frame(a = 1:5, b = 6:10, c = 11:15))

  result <- lake$from("data")$
    select(a, c)$
    run()

  expect_equal(names(result), c("a", "c"))
  expect_equal(ncol(result), 2)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_select"), recursive = TRUE)
})

test_that("QueryBuilder group_by() and summarize() work", {
  lake <- Lake$new("test_qb_group")

  lake$put("sales", data.frame(
    region = c("North", "North", "South", "South", "East"),
    amount = c(100, 150, 200, 250, 300),
    stringsAsFactors = FALSE
  ))

  result <- lake$from("sales")$
    group_by(region)$
    summarize(total = sum(amount, na.rm = TRUE))$
    run()

  expect_true("region" %in% names(result))
  expect_true("total" %in% names(result))
  expect_equal(nrow(result), 3)  # 3 regions

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_group"), recursive = TRUE)
})

test_that("QueryBuilder order_by() sorts data", {
  lake <- Lake$new("test_qb_order")

  lake$put("data", data.frame(
    x = c(3, 1, 4, 1, 5),
    y = c("c", "a", "d", "b", "e"),
    stringsAsFactors = FALSE
  ))

  result <- lake$from("data")$
    order_by(x)$
    run()

  expect_equal(result$x, c(1, 1, 3, 4, 5))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_order"), recursive = TRUE)
})

test_that("QueryBuilder limit() restricts rows", {
  lake <- Lake$new("test_qb_limit")

  lake$put("data", data.frame(x = 1:100))

  result <- lake$from("data")$
    limit(10)$
    run()

  expect_equal(nrow(result), 10)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_limit"), recursive = TRUE)
})

test_that("QueryBuilder top() gets top N by column", {
  lake <- Lake$new("test_qb_top")

  lake$put("scores", data.frame(
    name = c("Alice", "Bob", "Carol", "Dave", "Eve"),
    score = c(85, 92, 78, 95, 88),
    stringsAsFactors = FALSE
  ))

  result <- lake$from("scores")$
    top(3, by = score)$
    run()

  expect_equal(nrow(result), 3)
  expect_equal(result$score[1], 95)  # Highest first

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_top"), recursive = TRUE)
})

test_that("QueryBuilder join() combines tables", {
  lake <- Lake$new("test_qb_join")

  lake$put("users", data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Carol"),
    stringsAsFactors = FALSE
  ))

  lake$put("orders", data.frame(
    user_id = c(1, 1, 2, 3),
    amount = c(100, 150, 200, 250),
    stringsAsFactors = FALSE
  ))

  result <- lake$from("orders")$
    left_join("users", on = c("user_id" = "id"))$
    run()

  expect_true("name" %in% names(result))
  expect_true("amount" %in% names(result))
  expect_equal(nrow(result), 4)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_join"), recursive = TRUE)
})

test_that("QueryBuilder as() saves to lake", {
  lake <- Lake$new("test_qb_as")

  lake$put("data", data.frame(x = 1:10))

  lake$from("data")$
    where(x > 5)$
    as("filtered")

  expect_true("filtered" %in% lake$tables()$table_name)

  result <- lake$get("filtered")
  expect_equal(nrow(result), 5)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_as"), recursive = TRUE)
})

test_that("QueryBuilder chaining works", {
  lake <- Lake$new("test_qb_chain")

  lake$put("data", data.frame(
    category = c("A", "A", "B", "B", "C"),
    value = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  ))

  result <- lake$from("data")$
    where(category != "C")$
    group_by(category)$
    summarize(avg = mean(value, na.rm = TRUE))$
    order_by(desc(avg))$
    run()

  expect_equal(nrow(result), 2)
  expect_equal(result$category[1], "B")  # B has higher average

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_chain"), recursive = TRUE)
})

test_that("QueryBuilder print() works", {
  lake <- Lake$new("test_qb_print")

  lake$put("data", data.frame(x = 1:5))

  qb <- lake$from("data")$
    where(x > 2)$
    select(x)$
    limit(10)

  # Should not error
  expect_output(print(qb), "QueryBuilder")

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_qb_print"), recursive = TRUE)
})
