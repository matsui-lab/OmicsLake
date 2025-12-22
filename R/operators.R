#' @title Custom Query Operators
#' @description SQL-like operators for use in Lake queries.
#' These operators provide familiar SQL semantics for filtering data.
#'
#' @name operators
#' @examples
#' \dontrun{
#' # Pattern matching
#' df[df$gene %like% "MT-%", ]
#'
#' # Range filtering
#' df[df$value %between% c(10, 100), ]
#'
#' # Case-insensitive matching
#' df[df$name %ilike% "john%", ]
#' }
NULL

#' LIKE operator for SQL-style pattern matching
#'
#' Matches patterns using SQL LIKE syntax where:
#' - `%` matches any sequence of characters
#' - `_` matches any single character
#'
#' @param x Character vector to match against
#' @param pattern Pattern string (SQL LIKE syntax)
#' @return Logical vector
#' @export
#' @examples
#' genes <- c("MT-CO1", "MT-CO2", "ACTB", "GAPDH")
#' genes[genes %like% "MT-%"]
`%like%` <- function(x, pattern) {
  # Convert SQL LIKE pattern to regex
  regex_pattern <- pattern
  # Escape regex special chars except % and _

  regex_pattern <- gsub("([.^$+?{}\\[\\]\\\\|()])", "\\\\\\1", regex_pattern)
  # Convert SQL wildcards to regex

  regex_pattern <- gsub("%", ".*", regex_pattern)
  regex_pattern <- gsub("_", ".", regex_pattern)
  # Anchor the pattern

  regex_pattern <- paste0("^", regex_pattern, "$")
  grepl(regex_pattern, x, perl = TRUE)
}

#' Case-insensitive LIKE operator
#'
#' @param x Character vector to match against
#' @param pattern Pattern string (SQL LIKE syntax)
#' @return Logical vector
#' @export
#' @examples
#' names <- c("John", "JOHN", "johnny", "Jane")
#' names[names %ilike% "john%"]
`%ilike%` <- function(x, pattern) {
  regex_pattern <- pattern

  regex_pattern <- gsub("([.^$+?{}\\[\\]\\\\|()])", "\\\\\\1", regex_pattern)
  regex_pattern <- gsub("%", ".*", regex_pattern)
  regex_pattern <- gsub("_", ".", regex_pattern)
  regex_pattern <- paste0("^", regex_pattern, "$")
  grepl(regex_pattern, x, perl = TRUE, ignore.case = TRUE)
}

#' BETWEEN operator for range filtering
#'
#' @param x Numeric vector
#' @param range Numeric vector of length 2 (min, max), inclusive
#' @return Logical vector
#' @export
#' @examples
#' values <- 1:20
#' values[values %between% c(5, 15)]
`%between%` <- function(x, range) {
  if (length(range) != 2) {
    stop("range must be a vector of length 2 (min, max)", call. = FALSE)
  }
  x >= range[1] & x <= range[2]
}

#' NOT BETWEEN operator
#'
#' @param x Numeric vector
#' @param range Numeric vector of length 2 (min, max)
#' @return Logical vector
#' @export
`%!between%` <- function(x, range) {
  if (length(range) != 2) {
    stop("range must be a vector of length 2 (min, max)", call. = FALSE)
  }
  x < range[1] | x > range[2]
}

#' NOT IN operator
#'
#' @param x Vector
#' @param table Values to exclude
#' @return Logical vector
#' @export
#' @examples
#' letters[letters %!in% c("a", "e", "i", "o", "u")]
`%!in%` <- function(x, table) {
  !(x %in% table)
}

#' Regex match operator
#'
#' @param x Character vector
#' @param pattern Regular expression pattern
#' @return Logical vector
#' @export
#' @examples
#' genes <- c("ENSG00000001", "ENSG00000002", "MT-CO1")
#' genes[genes %regex% "^ENSG"]
`%regex%` <- function(x, pattern) {
  grepl(pattern, x, perl = TRUE)
}

#' Case-insensitive regex match
#'
#' @param x Character vector
#' @param pattern Regular expression pattern
#' @return Logical vector
#' @export
`%iregex%` <- function(x, pattern) {
  grepl(pattern, x, perl = TRUE, ignore.case = TRUE)
}

#' Check for NULL/NA values
#'
#' @param x Vector to check
#' @return Logical vector indicating NA values
#' @export
#' @examples
#' x <- c(1, NA, 3, NA, 5)
#' x[is_null(x)]
is_null <- function(x) {
  is.na(x)
}

#' Check for non-NULL/non-NA values
#'
#' @param x Vector to check
#' @return Logical vector indicating non-NA values
#' @export
is_not_null <- function(x) {
  !is.na(x)
}

#' Check if string starts with a prefix
#'
#' @param x Character vector
#' @param prefix Prefix to check for
#' @return Logical vector
#' @export
#' @examples
#' genes <- c("MT-CO1", "MT-CO2", "ACTB")
#' genes[starts_with_str(genes, "MT-")]
starts_with_str <- function(x, prefix) {
  startsWith(as.character(x), prefix)
}

#' Check if string ends with a suffix
#'
#' @param x Character vector
#' @param suffix Suffix to check for
#' @return Logical vector
#' @export
ends_with_str <- function(x, suffix) {
  endsWith(as.character(x), suffix)
}

#' Check if string contains a substring
#'
#' @param x Character vector
#' @param substring Substring to search for
#' @return Logical vector
#' @export
#' @examples
#' text <- c("hello world", "goodbye", "hello there")
#' text[contains_str(text, "hello")]
contains_str <- function(x, substring) {

  grepl(substring, x, fixed = TRUE)
}

#' Coalesce - return first non-NA value
#'
#' @param ... Vectors to coalesce
#' @return Vector with first non-NA values
#' @export
#' @examples
#' x <- c(NA, 2, NA)
#' y <- c(1, NA, 3)
#' coalesce(x, y)  # Returns c(1, 2, 3)
coalesce <- function(...) {
  dplyr::coalesce(...)
}

#' If-else with NA handling
#'
#' @param condition Logical vector
#' @param true Value when TRUE
#' @param false Value when FALSE
#' @param na Value when NA (default: NA)
#' @return Vector with conditional values
#' @export
if_else_na <- function(condition, true, false, na = NA) {
  result <- ifelse(condition, true, false)
  result[is.na(condition)] <- na
  result
}
