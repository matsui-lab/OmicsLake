# Tests for custom query operators

test_that("%like% matches patterns", {
  genes <- c("MT-CO1", "MT-CO2", "ACTB", "GAPDH", "MT-ND1")

  result <- genes[genes %like% "MT-%"]
  expect_equal(length(result), 3)
  expect_true(all(grepl("^MT-", result)))

  # Single character wildcard
  result <- genes[genes %like% "MT-CO_"]
  expect_equal(length(result), 2)
})

test_that("%like% handles special characters", {
  values <- c("test.value", "test_value", "testXvalue")

  # Dots should be literal

  result <- values[values %like% "test.%"]
  expect_equal(result, "test.value")

  # Underscore is single char wildcard
  result <- values[values %like% "test_value"]
  expect_equal(result, "test_value")
})

test_that("%ilike% is case insensitive", {
  names <- c("John", "JOHN", "johnny", "Jane", "Bob")

  result <- names[names %ilike% "john%"]
  expect_equal(length(result), 3)
  expect_true(all(tolower(result) %in% c("john", "john", "johnny")))
})

test_that("%between% filters ranges", {
  values <- 1:20

  result <- values[values %between% c(5, 15)]
  expect_equal(result, 5:15)
  expect_equal(length(result), 11)

  # Edge cases are inclusive
  expect_true(5 %in% result)
  expect_true(15 %in% result)
})

test_that("%!between% excludes ranges", {
  values <- 1:10

  result <- values[values %!between% c(4, 7)]
  expect_equal(result, c(1, 2, 3, 8, 9, 10))
})

test_that("%!in% excludes values", {
  letters_sample <- letters[1:10]

  vowels <- c("a", "e", "i", "o", "u")
  result <- letters_sample[letters_sample %!in% vowels]

  expect_false("a" %in% result)
  expect_false("e" %in% result)
  expect_true("b" %in% result)
})

test_that("%regex% matches regular expressions", {
  genes <- c("ENSG00000001", "ENSG00000002", "MT-CO1", "ENST00000001")

  result <- genes[genes %regex% "^ENSG"]
  expect_equal(length(result), 2)

  result <- genes[genes %regex% "\\d{8}"]
  expect_equal(length(result), 3)  # 8 consecutive digits
})

test_that("%iregex% is case insensitive regex", {
  values <- c("Hello", "HELLO", "hello", "world")

  result <- values[values %iregex% "^hello$"]
  expect_equal(length(result), 3)
})

test_that("is_null detects NA values", {
  x <- c(1, NA, 3, NA, 5)

  result <- is_null(x)
  expect_equal(result, c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(sum(is_null(x)), 2)
})

test_that("is_not_null detects non-NA values", {
  x <- c(1, NA, 3, NA, 5)

  result <- is_not_null(x)
  expect_equal(result, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(sum(is_not_null(x)), 3)
})

test_that("starts_with_str works", {
  genes <- c("MT-CO1", "MT-CO2", "ACTB", "GAPDH")

  result <- genes[starts_with_str(genes, "MT-")]
  expect_equal(length(result), 2)
})

test_that("ends_with_str works", {
  files <- c("data.csv", "results.csv", "config.json", "log.txt")

  result <- files[ends_with_str(files, ".csv")]
  expect_equal(length(result), 2)
})

test_that("contains_str works", {
  text <- c("hello world", "goodbye", "hello there", "hi")

  result <- text[contains_str(text, "hello")]
  expect_equal(length(result), 2)
})

test_that("operators work in filter expressions", {
  df <- data.frame(
    gene = c("MT-CO1", "MT-CO2", "ACTB", "GAPDH"),
    expr = c(100, 200, 50, 75),
    stringsAsFactors = FALSE
  )

  # Using %like% in filter
  result <- df[df$gene %like% "MT-%" & df$expr > 50, ]
  expect_equal(nrow(result), 2)

  # Using %between% in filter
  result <- df[df$expr %between% c(60, 150), ]
  expect_equal(nrow(result), 2)
})
