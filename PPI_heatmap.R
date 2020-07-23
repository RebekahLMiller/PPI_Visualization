## File containing functions for generating heatmaps showing COF/TF PPIs


## SET UP ----------------------------------------------------------------------


# Load necessary libraries
library("plyr")
library("tidyverse")


## DEFINE FUNCTIONS ------------------------------------------------------------


# Main function to plot heatmaps
# dat: the data frame to use
# columns: the name of the data frame column to plot as columns of the heatmap
# rows: the name of the data frame column to plot as rows of the heatmap
# filters: a list of filters to apply to the data frame (default: no filters)
#   See examples
# plot_title: the title for the heatmap (default: no title)
# x_lab: the x axis label (default: no label)
# y_lab: the y axis label (default: no label)
# show_legend: whether or not to show the legend (default: TRUE)
# legend_title: the title for the legend (default: NA)
# low_color (default: lightgrey)
# high_color (default: midnightblue)
plot_heatmap <-
    function(dat, columns, rows, filters = NA, plot_title = NULL,
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
        
        # Count number of times each pair of values to plot appears
        dat_aggregated <-
            aggregate(dat_filtered[c(rows, columns)],
                      by = list(dat_filtered[[columns]], dat_filtered[[rows]]),
                      FUN = length)
        
        # Generate heatmap
        dat_heatmap <-
            generate_heatmap(dat_aggregated, rows, plot_title, x_lab, y_lab,
                             show_legend, legend_title, low_color, high_color)
        
        # Return heatmap
        return(dat_heatmap)
    }

# Helper function to expand data frame rows when multiple values are listed in
#   a single row for one (or both) of the variables to plot
# Arguments are as explained for plot_heatmap()
expand_rows <- function(dat, columns, rows) {
    # Initialize return value to be original data frame
    dat_expanded <- dat
    
    # Expand rows based on Complex_A if necessary
    if (columns == "Complex_A" || rows == "Complex_A") {
        dat_expanded <- separate_rows(dat_expanded, Complex_A, sep = ";")
    }
    
    # Expand rows based on DBD_B if necessary
    if (columns == "DBD_B" || rows == "DBD_B") {
        dat_expanded <- separate_rows(dat_expanded, DBD_B, sep = "; ")
    }
    
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
            # Use str_detect to check if a cell contains a string
            dat_filtered <- dat_filtered %>%
                filter(
                    filters[[name]] %>%
                        # Search for each string individually
                        map(~ str_detect(dat_filtered[[name]],
                                         fixed(., ignore_case  = TRUE))) %>%
                        # Keep the row if any of the strings are found
                        pmap_lgl(any)
                )
        } else {
            # If this column is not character values, just check if the value
            #   for each row is in the list of values to include
            dat_filtered <-
                filter(dat_filtered, !!as.symbol(name) %in% filters[[name]])
        }
    }
    
    # Return filtered data frame
    return(dat_filtered)
}

# Helper function to generate a ggplot2 heatmap
# Arguments are as explained for plot_heatmap()
generate_heatmap <-
    function(dat, rows, plot_title, x_lab, y_lab, show_legend,
             legend_title, low_color, high_color) {
        # Start the plot
        heatmap_dat <-
            ggplot(dat, aes(x = Group.1, y = Group.2, fill = get(rows))) +
            geom_tile(colour = "white")
        
        # Remove axis ticks and grid lines and set theme to black and white
        heatmap_dat <- heatmap_dat +
            theme_bw() +
            theme(
                axis.ticks = element_blank(),
                axis.text.x = element_text(angle = 270, hjust = 0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
            )
        
        # Add axis labels and legend title
        heatmap_dat <- heatmap_dat +
            labs(x = x_lab, y = y_lab, fill = legend_title, title = plot_title)
        
        # Set color scheme
        heatmap_dat <- heatmap_dat +
            scale_fill_gradient(low = low_color, high = high_color)
        
        # Remove legend if show_legend is FALSE
        if (!show_legend) {
            heatmap_dat <- heatmap_dat +
                guides(fill = FALSE)
        }
        
        # Return the heatmap
        return(heatmap_dat)
    }

