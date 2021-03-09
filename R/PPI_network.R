## File containing functions for generating network plots showing COF/TF PPIs


# DEFINE FUNCTIONS -------------------------------------------------------------


# Main function to plot networks
# dat_igraph: the igraph object to plot
# layout: the layout for the network graph (default: "sphere")
# edge_style: the edge style for the network graph (default: "line")
#   must be one of ("line", "arc", NA)
# arc_curvature: the edge curvature to use when the edge style is "arc"
# plot_nodes: whether to plot the nodes (default: TRUE)
# node_color: a data frame with three columns:
#   column to color by, value in that column to color, color to use
#   (default: NA)
# node_label: the column to use to label the nodes (default: NA)
plot_network <-
    function(dat_igraph, layout = "sphere", edge_style = "line",
             arc_curvature = 0.1, plot_nodes = TRUE, node_color = NA,
             node_label = NA) {
        # Set all nodes to black initially
        igraph::V(dat_igraph)$color <- "black"

        # Change colors of nodes listed in node_color data frame
        if (is.data.frame(node_color)) {
            lapply(1:nrow(node_color), function(x) {
                # Find all nodes in dat_igraph that match the info in this row
                node_rows <-
                    which(igraph::vertex_attr(dat_igraph, node_color[x, 1])
                          == node_color[x, 2])

                # Change the color for the matching nodes
                igraph::V(dat_igraph)$color[node_rows] <<- node_color[x, 3]
            })
        }

        # Start the plot
        dat_network <- ggraph::ggraph(dat_igraph, layout = layout)

        # Add edges
        if (!is.na(edge_style)) {
            dat_network <- dat_network +
                switch(
                    edge_style,
                    "line" = ggraph::geom_edge_link(),
                    "arc" = ggraph::geom_edge_arc(curvature = arc_curvature)
                )
        }

        # Add nodes
        if (plot_nodes) {
            dat_network <- dat_network +
                ggraph::geom_node_point(color = igraph::V(dat_igraph)$color)
        }

        # Add node labels
        if (!(is.na(node_label) | node_label == "None")) {
            dat_network <- dat_network +
                ggraph::geom_node_label(ggplot2::aes(label = replace_na(
                    igraph::vertex_attr(dat_igraph, node_label),
                    ""
                )),
                repel = TRUE)
        }

        # Set theme to theme_void() to remove axes and background
        dat_network <- dat_network + ggplot2::theme_void()

        # Maybe should add an optional legend here for the colors?

        # Return the finished network plot
        return(dat_network)
    }

# Main function to generate igraph object from PPI table
# dat: the PPI data frame to use
extract_igraph <- function(dat) {
    # Make sure there are rows in the data frame
    if (nrow(dat) == 0) {
        return()
    }

    # Extract edge data frame
    edges <- extract_edges(dat)

    # Extract node data frame
    nodes <- extract_nodes(dat)

    # Generate igraph object
    net <- igraph::graph_from_data_frame(edges, vertices = nodes)

    # Fix the name of the first node attribute
    names(igraph::vertex_attr(net))[1] <- colnames(nodes)[1]

    # Return igraph object
    return(net)
}

# Helper function to generate edge data frame from PPI table
# Arguments are as explained for extract_igraph()
extract_edges <- function(dat) {
    # Find columns whose names end in "_A" (these contain node information)
    nodes_a <- grep("_A", colnames(dat), fixed = TRUE)

    # Find columns whose names end in "_B" (these contain node information)
    nodes_b <- grep("_B", colnames(dat), fixed = TRUE)

    # Find remaining columns (these contain edge information)
    edge_cols <- setdiff(1:ncol(dat), nodes_a)
    edge_cols <- setdiff(edge_cols, nodes_b)

    # Remove all node information columns except the first of each "_A" and "_B"
    edge_dat <- dat[c(nodes_a[1], nodes_b[1], edge_cols)]

    # Return trimmed data frame
    return(edge_dat)
}

# Helper function to generate node data frame from PPI table
# Arguments are as explained for extract_igraph()
extract_nodes <- function(dat) {
    # Extract columns whose names end in "_A" (these contain node information)
    nodes_a <- dat[grep("_A", colnames(dat), fixed = TRUE)]

    # Extract columns whose names end in "_B" (these contain node information)
    nodes_b <- dat[grep("_B", colnames(dat), fixed = TRUE)]

    # Remove "_A" and "_B" from column names
    colnames(nodes_a) <-
        vapply(colnames(nodes_a), function(x) {
            return(substr(x, 1, nchar(x) - 2))
        }, character(1))
    colnames(nodes_b) <-
        vapply(colnames(nodes_b), function(x) {
            return(substr(x, 1, nchar(x) - 2))
        }, character(1))

    # Combine data frames of node A and node B information
    nodes <- plyr::rbind.fill(nodes_a, nodes_b)

    # Remove duplicate rows from data frame of node information
    nodes <- dplyr::distinct(nodes)

    # Collapse rows so each node has only one row (adds a column at the front)
    nodes <- aggregate.data.frame(nodes,
                                  by = list(nodes[, 1]),
                                  FUN = collapse_rows)

    # Return combined, deduplicated data frame (removing extra column at front)
    return(nodes[-1])
}

# Helper function to collapse a vector of values into a single character vector
# x: the vector of values to collapse
collapse_rows <- function(x) {
    # Keep only one copy of each value in the list
    unique_x <- unique(x)

    # Return NA instead of an empty string if unique_x contains only NA
    if (all(is.na(unique_x))) {
        return(NA)
    }

    # Print values with a semicolon delimiter (removing NA values)
    return(paste(unique_x[!is.na(unique_x)], collapse = ";"))
}

