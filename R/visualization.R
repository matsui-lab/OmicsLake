#' Visualize dependency lineage as a graph
#'
#' Creates a visualization of the dependency relationships between tables and
#' objects in the OmicsLake project. Requires the igraph package to be installed.
#'
#' @param name Name of the table or object to visualize
#' @param direction Direction to traverse: "upstream" (dependencies),
#'   "downstream" (dependents), or "both"
#' @param layout Graph layout algorithm: "tree", "sugiyama" (hierarchical),
#'   "circle", or "auto"
#' @param max_depth Maximum depth to traverse (default: 10)
#' @param project Project name
#' @param vertex.size Size of graph nodes (default: 20)
#' @param vertex.label.cex Size of node labels (default: 0.8)
#' @param edge.arrow.size Size of edge arrows (default: 0.5)
#' @param main Plot title (auto-generated if NULL)
#' @param ... Additional arguments passed to plot.igraph()
#'
#' @return An igraph object (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' ol_init("myproject")
#' ol_save("raw", data)
#' ol_save("processed", result, depends_on = "raw")
#' 
#' ol_plot_lineage("processed", direction = "upstream")
#' 
#' ol_plot_lineage("processed", direction = "upstream", layout = "circle")
#' }
ol_plot_lineage <- function(
  name,
  direction = c("upstream", "downstream", "both"),
  layout = c("tree", "sugiyama", "circle", "auto"),
  max_depth = 10,
  project = getOption("ol.project"),
  vertex.size = 20,
  vertex.label.cex = 0.8,
  edge.arrow.size = 0.5,
  main = NULL,
  ...
) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for visualization. Install it with: install.packages('igraph')", call. = FALSE)
  }
  
  direction <- match.arg(direction)
  layout <- match.arg(layout)
  
  if (direction == "both") {
    lineage_up <- ol_show_lineage(name, direction = "upstream", max_depth = max_depth, project = project)
    lineage_down <- ol_show_lineage(name, direction = "downstream", max_depth = max_depth, project = project)
    
    if (nrow(lineage_up) == 0 && nrow(lineage_down) == 0) {
      message("No dependencies found for '", name, "'")
      return(invisible(NULL))
    }
    
    if (nrow(lineage_up) > 0) {
      lineage_up <- lineage_up[, c("child", "parent", "parent_type", "relationship")]
      names(lineage_up)[3] <- "type"
    }
    if (nrow(lineage_down) > 0) {
      lineage_down <- lineage_down[, c("parent", "child", "child_type", "relationship")]
      names(lineage_down) <- c("parent", "child", "type", "relationship")
    }
    
    if (nrow(lineage_up) > 0 && nrow(lineage_down) > 0) {
      lineage <- rbind(lineage_up, lineage_down)
    } else if (nrow(lineage_up) > 0) {
      lineage <- lineage_up
    } else {
      lineage <- lineage_down
    }
  } else {
    lineage <- ol_show_lineage(name, direction = direction, max_depth = max_depth, project = project)
    
    if (nrow(lineage) == 0) {
      message("No dependencies found for '", name, "' in direction '", direction, "'")
      return(invisible(NULL))
    }
    
    if (direction == "upstream") {
      lineage <- lineage[, c("child", "parent", "parent_type", "relationship")]
      names(lineage)[3] <- "type"
    } else {
      lineage <- lineage[, c("parent", "child", "child_type", "relationship")]
      names(lineage)[3] <- "type"
    }
  }
  
  g <- .ol_lineage_to_igraph(lineage, name)
  
  if (layout == "auto") {
    layout_func <- igraph::layout_with_sugiyama
  } else if (layout == "tree") {
    layout_func <- igraph::layout_as_tree
  } else if (layout == "sugiyama") {
    layout_func <- igraph::layout_with_sugiyama
  } else if (layout == "circle") {
    layout_func <- igraph::layout_in_circle
  }
  
  if (is.null(main)) {
    main <- sprintf("Dependency Lineage: %s (%s)", name, direction)
  }
  
  plot(
    g,
    layout = layout_func,
    vertex.size = vertex.size,
    vertex.label.cex = vertex.label.cex,
    edge.arrow.size = edge.arrow.size,
    main = main,
    ...
  )
  
  invisible(g)
}

#' Convert lineage data.frame to igraph object
#' @param lineage Data.frame with parent, child, type, relationship columns
#' @param focal_node Name of the focal node (will be highlighted)
#' @return An igraph object
#' @keywords internal
.ol_lineage_to_igraph <- function(lineage, focal_node) {
  edges <- lineage[, c("parent", "child")]
  
  parent_nodes <- data.frame(
    name = unique(lineage$parent),
    stringsAsFactors = FALSE
  )
  child_nodes <- data.frame(
    name = unique(lineage$child),
    type = lineage$type[match(unique(lineage$child), lineage$child)],
    stringsAsFactors = FALSE
  )
  
  all_nodes <- unique(rbind(
    data.frame(name = parent_nodes$name, stringsAsFactors = FALSE),
    data.frame(name = child_nodes$name, stringsAsFactors = FALSE)
  ))
  
  all_nodes$type <- sapply(all_nodes$name, function(n) {
    idx <- which(child_nodes$name == n)
    if (length(idx) > 0) {
      return(child_nodes$type[idx[1]])
    }
    parent_idx <- which(lineage$parent == n)
    if (length(parent_idx) > 0) {
      return("object")
    }
    return("unknown")
  })
  
  g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = all_nodes)
  
  colors <- ifelse(igraph::V(g)$type == "table", "lightblue", "orange")
  colors[igraph::V(g)$name == focal_node] <- "red"
  igraph::V(g)$color <- colors
  
  igraph::E(g)$label <- ""
  
  return(g)
}
