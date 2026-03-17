#!/usr/bin/env Rscript
# === Figure 7: Key Results Dashboard ===
# OmicsLake Paper - Major Results Summary
#
# Panels:
#   A. Common scope attainment (target-normalized)
#   B. Mode-specific attainment (auto/guided/limit)
#   C. Scope pass rates
#   D. Limit-boundary confusion matrix

library(ggplot2)
library(dplyr)
library(scales)

COLORS_SCOPE <- c(
    common = "#1f77b4",
    auto_rollback = "#2ca02c",
    guided_manual = "#ff7f0e",
    limit_boundary = "#9467bd"
)

COLORS_PASS <- c(`TRUE` = "#27ae60", `FALSE` = "#e74c3c")

.figure7_theme <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
        theme(
            text = element_text(family = "sans"),
            plot.title = element_text(size = base_size + 1, face = "bold"),
            plot.subtitle = element_text(size = base_size - 1, color = "gray35"),
            axis.title = element_text(size = base_size, face = "bold"),
            axis.text = element_text(size = base_size - 1),
            panel.grid.major.y = element_line(color = "gray88", linewidth = 0.3),
            panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = base_size - 1, face = "bold"),
            legend.text = element_text(size = base_size - 1),
            plot.margin = margin(8, 10, 8, 8, unit = "pt")
        )
}

.figure7_resolve_paper_dir <- function() {
    if (file.exists(file.path("..", "results_unified_scorecard.csv"))) {
        return("..")
    }
    if (file.exists(file.path("inst", "paper", "results_unified_scorecard.csv"))) {
        return(file.path("inst", "paper"))
    }
    stop(
        "Cannot find results_unified_scorecard.csv. Run from inst/paper/figures",
        " or package root after scorecard generation.",
        call. = FALSE
    )
}

.figure7_read_required <- function(path) {
    if (!file.exists(path)) {
        stop("Missing required file: ", path, call. = FALSE)
    }
    read.csv(path, stringsAsFactors = FALSE)
}

.figure7_read_optional <- function(path) {
    if (!file.exists(path)) {
        return(NULL)
    }
    read.csv(path, stringsAsFactors = FALSE)
}

.figure7_metric_label <- function(x) {
    out <- gsub("_", " ", x, fixed = TRUE)
    tools::toTitleCase(out)
}

.figure7_attainment <- function(value, target, direction) {
    eps <- 1e-12
    ifelse(
        direction == "ge",
        value / pmax(target, eps),
        target / pmax(value, eps)
    )
}

.figure7_prepare_scorecard <- function(scorecard) {
    scorecard %>%
        mutate(
            scope = as.character(scope),
            metric_label = .figure7_metric_label(metric),
            attainment = .figure7_attainment(as.numeric(value), as.numeric(target),
                as.character(direction)),
            attainment = pmin(attainment, 2),
            pass = as.logical(pass)
        )
}

.figure7_panel_attainment <- function(df, title, subtitle) {
    if (!nrow(df)) {
        return(ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data",
                size = 4) +
            theme_void() +
            labs(title = title, subtitle = subtitle))
    }
    df <- df %>%
        arrange(attainment) %>%
        mutate(metric_label = factor(metric_label, levels = metric_label))
    xmax <- max(1.2, max(df$attainment, na.rm = TRUE) * 1.15)
    ggplot(df, aes(x = attainment, y = metric_label, fill = pass)) +
        geom_col(width = 0.7, color = "gray35", linewidth = 0.3) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "gray35") +
        geom_text(
            aes(label = sprintf("%.2fx", attainment)),
            hjust = -0.1,
            size = 3
        ) +
        scale_fill_manual(values = COLORS_PASS, labels = c("Fail", "Pass")) +
        coord_cartesian(xlim = c(0, xmax), clip = "off") +
        labs(
            title = title,
            subtitle = subtitle,
            x = "Target Attainment Ratio (1.0 = target)",
            y = NULL,
            fill = "Status"
        ) +
        .figure7_theme()
}

.figure7_panel_scope <- function(scope_summary) {
    df <- scope_summary %>%
        mutate(
            scope = as.character(scope),
            scope_label = .figure7_metric_label(scope),
            pass_rate = as.numeric(pass_rate)
        )
    ggplot(df, aes(x = reorder(scope_label, pass_rate), y = pass_rate,
        fill = scope)) +
        geom_col(width = 0.7, color = "gray35", linewidth = 0.3) +
        geom_text(
            aes(label = sprintf("%.0f%%", 100 * pass_rate)),
            vjust = -0.5,
            size = 3
        ) +
        scale_fill_manual(values = COLORS_SCOPE, guide = "none") +
        scale_y_continuous(
            limits = c(0, 1.05),
            labels = percent_format(accuracy = 1)
        ) +
        labs(
            title = "C) Scope-Level Pass Rates",
            subtitle = "Pass ratio across common and mode-specific criteria",
            x = NULL,
            y = "Pass Rate"
        ) +
        .figure7_theme() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

.figure7_panel_confusion <- function(limit_conf) {
    if (is.null(limit_conf) || !nrow(limit_conf)) {
        return(ggplot() +
            annotate(
                "text",
                x = 0.5,
                y = 0.5,
                label = "results_limit_confusion_matrix.csv\nnot available",
                size = 4,
                lineheight = 1.2
            ) +
            theme_void() +
            labs(
                title = "D) Limit-Boundary Detection",
                subtitle = "Confusion matrix unavailable"
            ))
    }
    df <- limit_conf %>%
        mutate(
            truth = factor(as.character(truth), levels = c("breakage",
                "no_breakage")),
            predicted = factor(as.character(predicted), levels = c("detected",
                "not_detected")),
            count = as.numeric(count)
        )
    total <- sum(df$count, na.rm = TRUE)
    diag_n <- sum(df$count[df$truth == "breakage" & df$predicted == "detected"],
        na.rm = TRUE) +
        sum(df$count[df$truth == "no_breakage" &
            df$predicted == "not_detected"], na.rm = TRUE)
    acc <- if (total > 0) diag_n / total else NA_real_
    ggplot(df, aes(x = predicted, y = truth, fill = count)) +
        geom_tile(color = "white", linewidth = 0.8) +
        geom_text(aes(label = as.integer(count)), size = 5, fontface = "bold") +
        scale_fill_gradient(low = "#deebf7", high = "#08519c") +
        labs(
            title = "D) Limit-Boundary Detection",
            subtitle = sprintf("Confusion Matrix (accuracy: %.0f%%)", 100 * acc),
            x = "Predicted",
            y = "Ground Truth",
            fill = "Count"
        ) +
        .figure7_theme()
}

create_figure7 <- function(scorecard, scope_summary, limit_conf = NULL) {
    df <- .figure7_prepare_scorecard(scorecard)
    common <- df %>% filter(scope == "common")
    mode_specific <- df %>% filter(scope != "common")

    p_a <- .figure7_panel_attainment(
        common,
        title = "A) Common Reproducibility Axes",
        subtitle = "State restore, lineage, cross-env, stability, breakage handling"
    )
    p_b <- .figure7_panel_attainment(
        mode_specific,
        title = "B) Mode-Specific and Limit Axes",
        subtitle = "Auto-rollback, guided-manual, and limit-boundary metrics"
    )
    p_c <- .figure7_panel_scope(scope_summary)
    p_d <- .figure7_panel_confusion(limit_conf)

    list(panel_a = p_a, panel_b = p_b, panel_c = p_c, panel_d = p_d)
}

generate_figure7 <- function(output_dir = "output") {
    paper_dir <- .figure7_resolve_paper_dir()
    scorecard <- .figure7_read_required(
        file.path(paper_dir, "results_unified_scorecard.csv")
    )
    scope_summary <- .figure7_read_required(
        file.path(paper_dir, "results_unified_scope_summary.csv")
    )
    limit_conf <- .figure7_read_optional(
        file.path(paper_dir, "results_limit_confusion_matrix.csv")
    )
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    panels <- create_figure7(scorecard, scope_summary, limit_conf)
    p_a <- panels$panel_a
    p_b <- panels$panel_b
    p_c <- panels$panel_c
    p_d <- panels$panel_d

    panel_paths <- c(
        file.path(output_dir, "figure7A_common_axes.pdf"),
        file.path(output_dir, "figure7B_mode_axes.pdf"),
        file.path(output_dir, "figure7C_scope_pass_rates.pdf"),
        file.path(output_dir, "figure7D_limit_confusion.pdf")
    )
    ggsave(panel_paths[[1]], p_a, width = 8, height = 5, device = "pdf")
    ggsave(panel_paths[[2]], p_b, width = 8, height = 5, device = "pdf")
    ggsave(panel_paths[[3]], p_c, width = 7, height = 4.5, device = "pdf")
    ggsave(panel_paths[[4]], p_d, width = 6, height = 4.5, device = "pdf")

    if (!requireNamespace("patchwork", quietly = TRUE)) {
        message("patchwork not available; generated panel files only.")
        return(invisible(list(panels = panel_paths, dashboard = NULL)))
    }

    dashboard <- (p_a + p_b) / (p_c + p_d) +
        patchwork::plot_annotation(
            title = "OmicsLake Major Results Dashboard",
            subtitle = paste(
                "Unified scorecard across common/mode/limit scopes.",
                "Target attainment ratio >= 1 indicates criterion satisfied."
            ),
            caption = "Source: inst/paper/results_*.csv"
        ) &
        theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray35"),
            plot.caption = element_text(size = 9, color = "gray40")
        )

    dashboard_pdf <- file.path(output_dir, "figure7_key_results_dashboard.pdf")
    dashboard_png <- file.path(output_dir, "figure7_key_results_dashboard.png")
    ggsave(dashboard_pdf, dashboard, width = 15, height = 10, device = "pdf")
    ggsave(dashboard_png, dashboard, width = 15, height = 10, dpi = 300)

    message("Generated Figure 7 outputs:")
    message("  - ", dashboard_pdf)
    message("  - ", dashboard_png)
    message("  - ", paste(basename(panel_paths), collapse = ", "))
    invisible(list(panels = panel_paths, dashboard = c(dashboard_pdf,
        dashboard_png)))
}

.figure7_is_direct_run <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (!length(file_arg)) {
        return(FALSE)
    }
    script <- sub("^--file=", "", file_arg[[1]])
    identical(basename(script), "figure7_key_results_dashboard.R")
}

if (!interactive() && .figure7_is_direct_run()) {
    generate_figure7("output")
}
