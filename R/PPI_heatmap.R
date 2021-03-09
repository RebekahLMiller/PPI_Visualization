## File containing functions for generating heatmaps showing COF/TF PPIs


# DEFINE FUNCTIONS -------------------------------------------------------------


# Main function to plot heatmaps
# dat: the data frame to use
# columns: the name of the data frame column to plot as columns of the heatmap
# rows: the name of the data frame column to plot as rows of the heatmap
# fill_var: the name of the data frame column to use to color the heatmap
#   (default: color by number of interactions)
# filters: a list of filters to apply to the data frame (default: no filters)
#   should be a named list where the name is the column to search and the values
#   are the values to search for in that column
#   i.e., filters = list("Complex_A" = c("MLL", "PRC2"), "DBD_B" = "C2H2 ZF")
# cluster_cols: whether the columns should be ordered by hierarchical clustering
#   (default: FALSE)
# cluster_rows: wheher the rows should be ordered by hierarchical clustering
#   (default: FALSE)
# plot_title: the title for the heatmap (default: no title)
# x_lab: the x axis label (default: no label)
# y_lab: the y axis label (default: no label)
# show_legend: whether or not to show the legend (default: TRUE)
# legend_title: the title for the legend (default: no title)
# low_color: the color at the low end of the gradient (default: lightgrey)
# high_color: the color at the high end of the gradient (default: midnightblue)
plot_heatmap <-
    function(dat, columns, rows, fill_var = NA, filters = NA,
             cluster_cols = FALSE, cluster_rows = FALSE, plot_title = NULL,
             x_lab = NULL, y_lab = NULL, show_legend = TRUE, legend_title = NA,
             low_color = "lightgrey", high_color = "midnightblue") {
        # Depending on which columns are selected, expand rows
        dat_expanded <- expand_rows(dat, columns, rows)

        # Filter out unwanted rows
        dat_filtered <- filter_rows(dat_expanded, filters)

        # Check if there are any rows left
        if (nrow(dat_filtered) == 0) {
            return()
        }

        # Extract new data frame with column, row, and fill values
        if (is.na(fill_var)) {
            # Count number of times each pair of values to plot appears
            dat_aggregated <-
                aggregate(
                    dat_filtered[1],
                    by = list(dat_filtered[[columns]], dat_filtered[[rows]]),
                    FUN = length
                )

            # Fix the column names of the new data frame
            colnames(dat_aggregated) <-
                make.unique(c(columns, rows, "number_interactions"))
        } else {
            # Take the average value in fill_var column
            dat_aggregated <-
                aggregate(
                    dat_filtered[fill_var],
                    by = list(dat_filtered[[columns]], dat_filtered[[rows]]),
                    # aggregate() turns things into factors, so undo that
                    FUN = function(x) {
                        mean(as.numeric(as.character(x)))
                    }
                )

            # Fix the column names of the new data frame
            colnames(dat_aggregated) <-
                make.unique(c(columns, rows, fill_var))
        }

        # Generate heatmap
        dat_heatmap <-
            generate_heatmap(dat_aggregated, cluster_cols, cluster_rows,
                             plot_title, x_lab, y_lab,
                             show_legend, legend_title, low_color, high_color)

        # Return heatmap
        return(dat_heatmap)
    }

# Helper function to expand data frame rows when multiple values are listed in
#   a single row for one (or both) of the variables to plot
# Values must be semicolon delimited
# Arguments are as explained for plot_heatmap()
expand_rows <- function(dat, columns, rows) {
    # Separate rows based on variable to plot as columns
    dat_expanded <- tidyr::separate_rows(dat, !!as.symbol(columns), sep = ";")

    # Separate rows based on variable to plot as rows
    dat_expanded <-
        tidyr::separate_rows(dat_expanded,!!as.symbol(rows), sep = ";")

    # Return expanded data frame
    return(dat_expanded)
}

# Helper function to filter the data frame rows to use for plotting
# Arguments are as explained for plot_heatmap()
filter_rows <- function(dat, filters) {
    # Initialize return value to be original data frame
    dat_filtered <- dat

    # Return the original data frame if filters is NA
    if (all(is.na(filters))) {
        return(dat_filtered)
    }

    # Remove columns to filter by that are not present in the data frame
    filters <- filters[names(filters) %in% colnames(dat_filtered)]

    # Loop through the columns to filter by
    for (name in names(filters)) {
        # Check if this column contains characters
        if (is.character(dat_filtered[[name]])) {
            # Put together a regular expression containing all the search terms
            pattern <-
                paste(paste0("(^|;)", filters[[name]], "($|;)"), collapse = "|")

            # Check each cell in the column for the regular expression
            dat_filtered <-
                dplyr::filter(dat_filtered,
                       stringr::str_detect(dat_filtered[[name]], pattern))

        } else {
            # If this column is not character values, just check if the value
            #   for each row is in the list of values to include
            dat_filtered <-
                dplyr::filter(
                    dat_filtered,
                    !!as.symbol(name) %in% filters[[name]]
                )
        }
    }

    # Return filtered data frame
    return(dat_filtered)
}

# Helper function to generate a ggplot2 heatmap
# Arguments are as explained for plot_heatmap()
generate_heatmap <-
    function(dat, cluster_cols, cluster_rows, plot_title, x_lab, y_lab,
             show_legend, legend_title, low_color, high_color) {
        # Get the names of the columns of the data frame
        col_names <- names(dat)

        # Change the column order if the columns should be clustered
        if (cluster_cols & length(unique(dat[[col_names[1]]])) > 1) {
            # Convert data frame to wide format
            dat_mat <-
                reshape2::dcast(dat, get(col_names[1]) ~ get(col_names[2]),
                      value.var = col_names[3]) %>%
                # Replace NAs with zeroes
                replace(is.na(.), 0) %>%
                # Convert the first column to row names
                tibble::column_to_rownames("get(col_names[1])")

            # Figure out column order using hierarchical clustering
            col_order <- hclust(dist(dat_mat))$order

            # Convert column into a factor with levels in the clustered order
            dat[[col_names[1]]] <-
                factor(dat[[col_names[1]]],
                       levels = rownames(dat_mat)[col_order])
        }

        # Change the row order if the rows should be clustered
        if (cluster_rows & length(unique(dat[[col_names[2]]])) > 1) {
            # Convert data frame to wide format
            dat_mat <-
                reshape2::dcast(dat, get(col_names[2]) ~ get(col_names[1]),
                      value.var = col_names[3]) %>%
                # Replace NAs with zeroes
                replace(is.na(.), 0) %>%
                # Convert the first column to row names
                tibble::column_to_rownames("get(col_names[2])")

            # Figure out row order using hierarchical clustering
            row_order <- hclust(dist(dat_mat))$order

            # Convert column into a factor with levels in the clustered order
            dat[[col_names[2]]] <-
                factor(dat[[col_names[2]]],
                       levels = rownames(dat_mat)[row_order])
        }

        # Start the plot
        heatmap_dat <-
            ggplot2::ggplot(dat, ggplot2::aes(
                x = get(col_names[1]),
                y = get(col_names[2]),
                fill = get(col_names[3])
            )) +
            ggplot2::geom_tile(colour = "white")

        # Remove axis ticks and grid lines and set theme to black and white
        heatmap_dat <- heatmap_dat +
            ggplot2::theme_bw() +
            ggplot2::theme(
                axis.ticks = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_text(angle = 270, hjust = 0),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank()
            )

        # Add axis labels and legend title
        heatmap_dat <- heatmap_dat +
            ggplot2::labs(x = x_lab,
                          y = y_lab,
                          fill = legend_title,
                          title = plot_title)

        # Set color scheme
        heatmap_dat <- heatmap_dat +
            ggplot2::scale_fill_gradient(low = low_color, high = high_color)

        # Remove legend if show_legend is FALSE
        if (!show_legend) {
            heatmap_dat <- heatmap_dat +
                ggplot2::guides(fill = FALSE)
        }

        # Return the heatmap
        return(heatmap_dat)
    }

