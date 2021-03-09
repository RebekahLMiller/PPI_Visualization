## File containing the shiny modules unique to the network tab


# GENERATE NETWORK -------------------------------------------------------------


# Define the UI for the network tab's plot generation
generate_network_UI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    shiny::tagList(
        # Select a layout for the network
        shiny::radioButtons(
            ns("layout"),
            "Select a network layout",
            choices = c("sphere", "lgl", "circle", "grid", "kk", "fr"),
            selected = "sphere"
        ),

        # Checkbox to determine if the nodes should be plotted
        shiny::checkboxInput(
            ns("plot_nodes"),
            "Should the nodes be plotted?",
            value = 1
        ),

        # Show an additional panel if the nodes are plotted
        shiny::conditionalPanel(
            "input.plot_nodes == 1",

            # Set the namespace
            ns = ns,

            # Placeholder for color filters
            tags$div(id = ns("colors_placeholder")),

            # Button to add color filters
            shiny::actionButton(ns("add_color"), "Add a node color"),

            # Button to remove color filters
            shiny::actionButton(ns("remove_color"), "Remove a node color")
        ),

        # Add a dropdown menu of columns to label by
        shiny::selectInput(
            ns("node_label"),
            "Choose a column to label the nodes",
            choices = NA
        ),

        # Select a style for the edges
        shiny::radioButtons(
            ns("edge_style"),
            "Select an edge style",
            choices = c("line", "arc", NA),
            selected = "line"
        ),

        # Show an additional panel if the edge style is "arc"
        shiny::conditionalPanel(
            "input.edge_style == 'arc'",

            # Set the namespace
            ns = ns,

            # Add a slider to set arc curvature
            shiny::sliderInput(
                ns("arc_curvature"),
                "Set the arc curvature",
                min = 0,
                max = 1,
                value = 0.1
            )
        )
    )
}

# Define the UI for the network tab's plot generation
generate_network_server <- function(id, dat_igraph) {
    shiny::moduleServer(id, function(input, output, session) {
        # Set the namespace
        ns = session$ns

        # Update the choices for node label whenever dat_igraph changes
        shiny::observeEvent(dat_igraph(), {
            # Update the node label dropdown
            shiny::updateSelectInput(
                session,
                "node_label",
                choices = c("None", igraph::vertex_attr_names(dat_igraph()))
            )
        })

        # Keep track of the current and previous number of colors
        num_colors <- shiny::reactiveVal(0)
        prev_num_colors <- shiny::reactiveVal(0)

        # Disable the remove color button if there are no colors
        shiny::observe({
            if (num_colors() == 0) {
                shinyjs::disable("remove_color")
            } else {
                shinyjs::enable("remove_color")
            }
        })

        # Add a color when the add color button is clicked
        shiny::observeEvent(input$add_color, {
            # Make ids for referencing the new color div and its contents
            n <- num_colors() + 1
            div_id <- paste("color_div", n, sep = "_")
            column_id <- paste("color_column", n, sep = "_")
            values_id <- paste("color_values", n, sep = "_")
            color_id <- paste("color_choice", n, sep = "_")

            # Add color selecting widgets
            shiny::insertUI(
                # Set the location for the new widgets
                paste0("#", ns("colors_placeholder")),
                "beforeEnd",

                # Define the UI for the new widgets
                tags$div(
                    # Add color header
                    h4(paste("Color", n)),

                    # Add dropdown menu to choose column to color by
                    shiny::selectInput(
                        ns(column_id),
                        "Select a variable to color by",
                        igraph::vertex_attr_names(dat_igraph())
                    ),

                    # Add dropdown menu to select terms to color
                    shiny::selectizeInput(
                        ns(values_id),
                        "Select the values to color",
                        choices = NULL,
                        multiple = TRUE
                    ),

                    # Choose the color for the matching nodes
                    colourpicker::colourInput(
                        ns(color_id),
                        "Pick the color for matching nodes",
                        value = sample(
                            colors()[grep(
                                "gr[a|e]y", colors(), invert = TRUE)], 1)),

                    # Give this color div an id so it can be removed easily
                    id = div_id
                ),

                # Insert the new widgets immediately
                immediate = TRUE
            )

            # Add one to the color counter
            num_colors(num_colors() + 1)
        })

        # Update the choices of terms to color nodes by based on selected column
        shiny::observe({
            if (num_colors() > prev_num_colors()) {
                for (n in 1:num_colors()) {
                    # Get the ids of the widgets for this div
                    column_id <- paste("color_column", n, sep = "_")
                    values_id <- paste("color_values", n, sep = "_")

                    # Update the dropdown for selecting the values
                    shiny::observeEvent(input[[column_id]], {
                        shiny::updateSelectizeInput(
                            session,
                            values_id,
                            choices = sort(unique(
                                igraph::vertex_attr(dat_igraph(),
                                                    input[[column_id]])
                            ))
                        )
                    })
                }
            }

            # Set the previous number of colors to the current number
            prev_num_colors(num_colors())
        })

        # Remove the last color when the remove color button is clicked
        shiny::observeEvent(input$remove_color, {
            # Clear the input values or the app won't actually update
            shiny::updateSelectizeInput(
                session,
                paste("network_color_values", num_colors(), sep = "_"),
                choices = character(0),
                selected = character(0)
            )

            # Remove color div
            shiny::removeUI(paste('#color_div', num_colors(), sep = "_"))

            # Subtract one from the color counter
            num_colors(num_colors() - 1)
        })

        # Convert the selected node colors into a data frame
        node_colors <- shiny::reactive({
            # Initialize an empty data frame with three columns
            node_color_df <- data.frame(matrix(nrow = 0, ncol = 3))

            # Add a row to the data frame for each node color
            lapply(1:num_colors(), function(x) {
                # Get the selected column for this div
                color_column <- input[[paste("color_column", x, sep = "_")]]

                # Get the selected values for this div
                color_values <- input[[paste("color_values", x, sep = "_")]]

                # Collapse the selected values into a comma-delimited string
                if (!is.null(color_values)) {
                    color_values <- paste(color_values, collapse = ",")
                }

                # Get the selected color for this div
                node_color <- input[[paste("color_choice", x, sep = "_")]]

                # Add a row to the data frame with the column, values, and color
                node_color_df <<-
                    rbind(node_color_df,
                          c(color_column, color_values, node_color))
            })

            # If there are no rows in the data frame, return NA
            if (nrow(node_color_df) == 0 || num_colors() == 0) {
                return(NA)
            }

            # Separate the rows so each value has its own row
            node_color_df <- tidyr::separate_rows(node_color_df, 2, sep = ",")

            # Return the completed data frame
            return(node_color_df)
        })

        # Generate the network plot
        network_plot <- shiny::reactive({
            # Check if dat_igraph is an igraph object
            if (!igraph::is.igraph(dat_igraph())) {
                return()
            }

            # Generate the plot
            network_plot <- plot_network(
                dat_igraph(),
                input$layout,
                input$edge_style,
                input$arc_curvature,
                input$plot_nodes,
                node_colors(),
                input$node_label
            )

            # Return the plot
            return(network_plot)
        })

        # Return the network plot
        return(network_plot)
    })
}


# DOWNLOAD NETWORK -------------------------------------------------------------


# The UI for the network tab's download section is defined in "reused_modules.R"

# Define the server for the network tab's download section
download_network_server <- function(id, network_plot) {
    shiny::moduleServer(id, function(input, output, session) {
        shiny::observeEvent(network_plot(), {
            # Decide on a pdf size based on the number of nodes
            plot_size <- max(nrow(network_plot()$data) / 3, 10)

            # Update the pdf width
            shiny::updateTextInput(
                session,
                "width",
                value = plot_size
            )

            # Update the pdf height
            shiny::updateTextInput(
                session,
                "height",
                value = plot_size
            )

            # Handle downloading the network as a pdf
            output$download <-
                shiny::downloadHandler(
                    "network.pdf",
                    function(file) {
                        # Save the pdf
                        ggplot2::ggsave(
                            file,
                            plot = network_plot(),
                            device = "pdf",
                            width = as.numeric(input$width),
                            height = as.numeric(input$height),
                            units = "cm",
                            limitsize = FALSE
                        )
                    }
                )
        })
    })
}

