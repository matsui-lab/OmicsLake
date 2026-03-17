#!/usr/bin/env Rscript

root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
description_path <- file.path(root, "DESCRIPTION")
if (!file.exists(description_path)) {
  stop("DESCRIPTION not found in current working directory: ", root)
}

d <- as.list(read.dcf(description_path)[1, ])

fail <- function(msg) {
  cat("FAIL:", msg, "\n")
  quit(status = 1)
}
warn <- function(msg) {
  cat("WARN:", msg, "\n")
}
ok <- function(msg) {
  cat("OK:", msg, "\n")
}

get_field <- function(name) {
  if (!name %in% names(d)) return(NA_character_)
  as.character(d[[name]])
}

required_fields <- c(
  "Package", "Version", "Title", "Description", "License",
  "Encoding", "Depends", "Imports", "Suggests",
  "VignetteBuilder", "biocViews", "BugReports"
)

for (f in required_fields) {
  val <- get_field(f)
  if (is.na(val) || !nzchar(val)) {
    fail(paste("Missing DESCRIPTION field:", f))
  }
}
ok("Required DESCRIPTION fields are present")

version <- get_field("Version")
parts <- strsplit(version, "\\.", fixed = FALSE)[[1]]
if (length(parts) != 3 || any(!grepl("^[0-9]+$", parts))) {
  fail(paste("Version must be x.y.z numeric, got:", version))
}
minor <- as.integer(parts[2])
if (minor %% 2 == 0) {
  warn(paste("Bioconductor devel usually expects odd minor version; current minor:", minor))
} else {
  ok(paste("Bioconductor-style odd minor version detected:", version))
}

if (!file.exists(file.path(root, "NEWS.md"))) {
  fail("NEWS.md is missing")
}
ok("NEWS.md found")

vignette_files <- list.files(file.path(root, "vignettes"), pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
if (length(vignette_files) == 0) {
  fail("No vignettes found under vignettes/")
}
ok(paste("Vignettes found:", length(vignette_files)))

has_english <- any(grepl("_EN\\.Rmd$", basename(vignette_files)))
if (!has_english) {
  warn("No explicit English vignette detected with *_EN.Rmd naming")
} else {
  ok("English vignette detected")
}

if (file.exists(file.path(root, ".Rbuildignore"))) {
  rb <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
  if (any(grepl("(^|/)vignettes\\(\\$\\|/\\)", rb))) {
    fail("vignettes/ is excluded in .Rbuildignore; remove it for Bioconductor")
  }
}
ok(".Rbuildignore does not exclude vignettes/")

cat("\nBioconductor readiness preflight completed.\n")
