test_that("ol_export_parquet exports table with default compression", {
  ol_init("test_export_default")
  
  test_data <- data.frame(
    id = 1:100,
    value = rnorm(100),
    category = rep(c("A", "B"), 50),
    stringsAsFactors = FALSE
  )
  ol_write("test_table", test_data)
  
  output_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(output_path), add = TRUE)
  
  result <- ol_export_parquet("test_table", output_path)
  
  expect_true(file.exists(output_path))
  expect_equal(normalizePath(result), normalizePath(output_path))
  expect_true(file.size(output_path) > 0)
})

test_that("ol_export_parquet works with different compression options", {
  ol_init("test_export_compression")
  
  test_data <- data.frame(
    id = 1:1000,
    text = rep(paste(rep("test", 100), collapse = " "), 1000),
    stringsAsFactors = FALSE
  )
  ol_write("test_table", test_data)
  
  compressions <- c("snappy", "zstd", "lz4", "brotli", "uncompressed")
  file_sizes <- numeric(length(compressions))
  
  for (i in seq_along(compressions)) {
    output_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(output_path), add = TRUE)
    
    ol_export_parquet("test_table", output_path, compression = compressions[i])
    
    expect_true(file.exists(output_path))
    file_sizes[i] <- file.size(output_path)
  }
  
  expect_true(file_sizes[5] > max(file_sizes[1:4]))
})

test_that("ol_export_parquet respects row_group_size parameter", {
  ol_init("test_export_row_group")
  
  test_data <- data.frame(
    id = 1:1000,
    value = rnorm(1000),
    stringsAsFactors = FALSE
  )
  ol_write("test_table", test_data)
  
  output_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(output_path), add = TRUE)
  
  ol_export_parquet("test_table", output_path, row_group_size = 250)
  
  expect_true(file.exists(output_path))
})

test_that("ol_export_parquet prevents overwriting by default", {
  ol_init("test_export_overwrite")
  
  test_data <- data.frame(x = 1:10)
  ol_write("test_table", test_data)
  
  output_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(output_path), add = TRUE)
  
  ol_export_parquet("test_table", output_path)
  
  expect_error(
    ol_export_parquet("test_table", output_path),
    "already exists"
  )
  
  expect_silent(ol_export_parquet("test_table", output_path, overwrite = TRUE))
})

test_that("ol_import_parquet creates table from Parquet file", {
  ol_init("test_import_create")
  
  test_data <- data.frame(
    gene_id = paste0("GENE", 1:50),
    expression = rnorm(50, mean = 100, sd = 20),
    stringsAsFactors = FALSE
  )
  ol_write("source_table", test_data)
  
  output_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(output_path), add = TRUE)
  
  ol_export_parquet("source_table", output_path)
  
  ol_import_parquet(output_path, "imported_table", mode = "create")
  
  imported <- ol_read("imported_table")
  expect_equal(nrow(imported), 50)
  expect_true("gene_id" %in% colnames(imported))
  expect_true("expression" %in% colnames(imported))
})

test_that("ol_import_parquet supports overwrite and append modes", {
  ol_init("test_import_modes")
  
  data1 <- data.frame(id = 1:10, value = 1:10, stringsAsFactors = FALSE)
  data2 <- data.frame(id = 11:20, value = 11:20, stringsAsFactors = FALSE)
  
  ol_write("data1", data1)
  ol_write("data2", data2)
  
  path1 <- tempfile(fileext = ".parquet")
  path2 <- tempfile(fileext = ".parquet")
  on.exit({unlink(path1); unlink(path2)}, add = TRUE)
  
  ol_export_parquet("data1", path1)
  ol_export_parquet("data2", path2)
  
  ol_import_parquet(path1, "target", mode = "create")
  expect_equal(nrow(ol_read("target")), 10)
  
  ol_import_parquet(path2, "target", mode = "overwrite")
  expect_equal(nrow(ol_read("target")), 10)
  expect_equal(min(ol_read("target")$id), 11)
  
  ol_import_parquet(path1, "target", mode = "append")
  expect_equal(nrow(ol_read("target")), 20)
})

test_that("ol_import_parquet handles multiple files", {
  ol_init("test_import_multiple")
  
  data1 <- data.frame(id = 1:10, value = 1:10, stringsAsFactors = FALSE)
  data2 <- data.frame(id = 11:20, value = 11:20, stringsAsFactors = FALSE)
  
  ol_write("data1", data1)
  ol_write("data2", data2)
  
  path1 <- tempfile(fileext = ".parquet")
  path2 <- tempfile(fileext = ".parquet")
  on.exit({unlink(path1); unlink(path2)}, add = TRUE)
  
  ol_export_parquet("data1", path1)
  ol_export_parquet("data2", path2)
  
  ol_import_parquet(c(path1, path2), "combined", mode = "create")
  
  result <- ol_read("combined")
  expect_equal(nrow(result), 20)
})

test_that("ol_import_parquet tracks dependencies", {
  ol_init("test_import_deps")
  
  source_data <- data.frame(x = 1:10)
  ol_write("source", source_data)
  
  parquet_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(parquet_path), add = TRUE)
  
  ol_export_parquet("source", parquet_path)
  ol_import_parquet(parquet_path, "imported", depends_on = "source")
  
  deps <- ol_get_dependencies("imported", direction = "upstream")
  expect_true(nrow(deps) > 0)
  expect_true("source" %in% deps$parent_name)
})

test_that("ol_export_parquet validates input parameters", {
  ol_init("test_export_validation")
  
  test_data <- data.frame(x = 1:10)
  ol_write("test_table", test_data)
  
  expect_error(
    ol_export_parquet("test_table", ""),
    "path must be a non-empty character string"
  )
  
  expect_error(
    ol_export_parquet("test_table", 123),
    "path must be a non-empty character string"
  )
  
  expect_error(
    ol_export_parquet("", tempfile()),
    "must be a non-empty character string"
  )
})

test_that("ol_import_parquet validates input parameters", {
  ol_init("test_import_validation")
  
  expect_error(
    ol_import_parquet("", "target"),
    "path must be a non-empty character vector"
  )
  
  expect_error(
    ol_import_parquet(tempfile(), ""),
    "must be a non-empty character string"
  )
  
  expect_error(
    ol_import_parquet(123, "target"),
    "path must be a non-empty character vector"
  )
})

test_that("roundtrip export and import preserves data", {
  ol_init("test_roundtrip")
  
  original_data <- data.frame(
    id = 1:100,
    name = paste0("Item", 1:100),
    value = rnorm(100),
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  ol_write("original", original_data)
  
  parquet_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(parquet_path), add = TRUE)
  
  ol_export_parquet("original", parquet_path, compression = "zstd")
  ol_import_parquet(parquet_path, "restored", mode = "create")
  
  restored_data <- ol_read("restored")
  
  expect_equal(nrow(restored_data), nrow(original_data))
  expect_equal(ncol(restored_data), ncol(original_data))
  expect_setequal(colnames(restored_data), colnames(original_data))
  
  for (col in colnames(original_data)) {
    expect_equal(restored_data[[col]], original_data[[col]], 
                info = paste("Column", col, "mismatch"))
  }
})
