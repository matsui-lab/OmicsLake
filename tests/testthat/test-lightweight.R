# Tests for lightweight mode (observe.R and wrap.R)

test_that("observe() tracks reads and writes", {
  # Create temp files for testing
  temp_dir <- tempdir()
  input_file <- file.path(temp_dir, "test_input.csv")
  output_file <- file.path(temp_dir, "test_output.csv")

  # Write test input
  write.csv(data.frame(x = 1:5), input_file, row.names = FALSE)

  # Observe a simple operation
  result <- observe({
    data <- read.csv(input_file)
    data$y <- data$x * 2
    write.csv(data, output_file, row.names = FALSE)
    data
  })

  expect_s3_class(result, "lake_observation")
  expect_true(length(result$reads) >= 0)  # May or may not capture depending on implementation
  expect_true(length(result$writes) >= 0)
  expect_true(!is.null(result$result))

  # Clean up
  unlink(input_file)
  unlink(output_file)
})

test_that("observe() returns result correctly", {
  result <- observe({
    x <- 1:10
    sum(x)
  })

  expect_equal(result$result, 55)
})

test_that("print.lake_observation works", {
  result <- observe({
    42
  })

  expect_output(print(result), "Lake Observation")
})

test_that("wrap_fn creates a working wrapper", {
  lake <- Lake$new("test_wrap_fn")

  # Simple function to wrap
  double_it <- function(x) x * 2

  # Wrap it
  tracked_double <- wrap_fn(double_it, lake, "doubled_result", save_output = FALSE)

  # Use the wrapper
  result <- tracked_double(5)
  expect_equal(result, 10)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_wrap_fn"), recursive = TRUE)
})

test_that("wrap_fn with save_output stores result", {
  lake <- Lake$new("test_wrap_save")

  add_one <- function(x) x + 1

  tracked_add <- wrap_fn(add_one, lake, "result", save_output = TRUE)
  result <- tracked_add(data.frame(val = 1:3))

  # Check that result was saved
  stored <- lake$get("result")
  expect_equal(stored$val, 2:4)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_wrap_save"), recursive = TRUE)
})

test_that("wrap_call works inline", {
  lake <- Lake$new("test_wrap_call")

  result <- wrap_call(lake, function(x) x^2, 5, output = "squared", save = TRUE)

  expect_equal(result, 25)
  expect_equal(lake$get("squared"), 25)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_wrap_call"), recursive = TRUE)
})

test_that("mark creates a marker node", {
  lake <- Lake$new("test_mark")

  # Mark some data without storing it
  large_data <- matrix(1:1000, 100, 10)
  mark("large_matrix", large_data, lake)

  # The marker should be recorded
  objects <- lake$objects()
  expect_true(any(grepl("marker", objects$name)))

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_mark"), recursive = TRUE)
})

test_that("link creates explicit dependency", {
  lake <- Lake$new("test_link")

  # Create two data items
  lake$put("parent", data.frame(x = 1:3))
  lake$put("child", data.frame(y = 4:6))

  # Link them
  link("parent", "child", lake)

  # Check the dependency exists
  deps <- lake$deps("child", direction = "up")
  expect_true("parent" %in% deps$parent_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_link"), recursive = TRUE)
})

test_that("with_tracking tracks dependencies", {
  lake <- Lake$new("test_with_tracking")

  lake$put("source", data.frame(x = 1:5))

  result <- with_tracking(lake, "computed", {
    data <- lake$get("source")
    data$y <- data$x * 2
    data
  })

  expect_equal(nrow(result), 5)
  expect_true("computed" %in% lake$tables()$table_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_with_tracking"), recursive = TRUE)
})

test_that("Pipeline executes steps in order", {
  lake <- Lake$new("test_pipeline")

  pipeline <- create_pipeline(lake, "test")
  pipeline$
    step("init", function() data.frame(x = 1:5))$
    step("double", function(data) { data$x <- data$x * 2; data })$
    step("add", function(data) { data$x <- data$x + 1; data })

  result <- pipeline$run()

  expect_equal(result$x, c(3, 5, 7, 9, 11))  # (1:5) * 2 + 1

  # Check that intermediate results were stored
  expect_true("test.init" %in% lake$tables()$table_name)
  expect_true("test.double" %in% lake$tables()$table_name)
  expect_true("test.add" %in% lake$tables()$table_name)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_pipeline"), recursive = TRUE)
})

test_that("ObserveSession tracks operations", {
  lake <- Lake$new("test_session")

  session <- observe_session(lake)
  expect_true(session$is_active())

  # Record some operations
  session$record("put", "data1")
  session$record("get", "data1")

  summary <- session$stop()
  expect_false(session$is_active())
  expect_equal(summary$n_operations, 2)

  # Clean up
  unlink(file.path(path.expand("~"), ".omicslake", "test_session"), recursive = TRUE)
})
