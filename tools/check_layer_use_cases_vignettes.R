#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
targets <- if (length(args)) {
    args
} else {
    c(
        "vignettes/omicslake_layer_use_cases.Rmd",
        "vignettes/omicslake_layer_use_cases_EN.Rmd"
    )
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("rmarkdown is required. Install with install.packages('rmarkdown').")
}

options(omicslake.vignette.eval = TRUE)

render_one <- function(path) {
    if (!file.exists(path)) {
        stop("Vignette file not found: ", path)
    }
    out_dir <- file.path(tempdir(), "omicslake-vignette-check")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- paste0(tools::file_path_sans_ext(basename(path)), ".html")
    cat("Rendering:", path, "\n")
    rmarkdown::render(
        input = path,
        output_format = "html_document",
        output_file = out_file,
        output_dir = out_dir,
        quiet = TRUE
    )
    cat("OK:", file.path(out_dir, out_file), "\n")
}

for (tg in targets) {
    render_one(tg)
}

cat("All layer use-case vignettes rendered with omicslake.vignette.eval=TRUE.\n")
