## File containing the shiny modules unique to the network tab


## GENERATE NETWORK ------------------------------------------------------------


# Define the UI for the network tab's plot generation
generate_network_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    tagList(
        # Select a layout for the network
        radioButtons(
            ns("layout"),
            "Select a network layout",
            choices = c("sphere", "lgl", "circle", "grid", "kk", "fr"),
            selected = "sphere"
        ),
        
        # Checkbox to determine if the nodes should be plotted
        checkboxInput(
            ns("plot_nodes"),
            "Should the nodes be plotted?",
            value = 1
        ),
        
        # Show an additional panel if the nodes are plotted
        conditionalPanel(
            "input.plot_nodes == 1",
            
            # Set the namespace
            ns = ns,
            
            # Placeholder for color filters
            tags$div(id = ns("colors_placeholder")),
            
            # Button to add color filters
            actionButton(ns("add_color"), "Add a node color"),
            
            # Button to remove color filters
            actionButton(ns("remove_color"), "Remove a node color")
        ),
        
        # Add a dropdown menu of columns to label by
        selectInput(
            ns("node_label"),
            "Choose a column to label the nodes",
            choices = NA
        ),
        
        # Select a style for the edges
        radioButtons(
            ns("edge_style"),
            "Select an edge style",
            choices = c("line", "arc", NA),
            selected = "line"
        ),
        
        # Show an additional panel if the edge style is "arc"
        conditionalPanel(
            "input.edge_style == 'arc'",
            
            # Set the namespace
            ns = ns,
            
            # Add a slider to set arc curvature
            sliderInput(
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
    moduleServer(id, function(input, output, session) {
        # Set the namespace
        ns = session$ns
        
        # Update the choices for node label whenever dat_igraph changes
        observeEvent(dat_igraph(), {
            # Update the node label dropdown
            updateSelectInput(
                session,
                "node_label",
                choices = c("None", vertex_attr_names(dat_igraph()))
            )
        })
        
        # Keep track of the number of colors
        num_colors <- reactiveVal(0)
        
        # Disable the remove color button if there are no colors
        observe({
            if (num_colors() == 0) {
                disable("remove_color")
            } else {
                enable("remove_color")
            }
        })
        
        # Keep track of the previous number of network colors
        prev_num_colors <- reactiveVal(0)
        
        # Add a color when the add color button is clicked
        observeEvent(input$add_color, {
            # Make ids for referencing the new color div and its contents
            n <- num_colors() + 1
            div_id <- paste("color_div", n, sep = "_")
            header_id <- paste("color_header", n, sep = "_")
            column_id <- paste("color_column", n, sep = "_")
            values_id <- paste("color_values", n, sep = "_")
            color_id <- paste("color_choice", n, sep = "_")
            
            # Add color selecting widgets
            insertUI(
                paste0("#", ns("colors_placeholder")),
                "beforeEnd",
                tags$div(
                    # Add color header
                    htmlOutput(ns(header_id)),
                    
                    # Add dropdown menu to choose column to color by
                    selectInput(
                        ns(column_id),
                        "Select a variable to color by",
                        vertex_attr_names(dat_igraph())
                    ),
                    
                    # Add dropdown menu to select terms to color
                    selectizeInput(
                        ns(values_id),
                        "Select the values to color",
                        choices = NULL,
                        multiple = TRUE
                    ),
                    
                    # Choose the color for the matching nodes
                    colourInput(
                        ns(color_id),
                        "Pick the color for matching nodes",
                        value = sample(
                            colors()[grep(
                                "gr[a|e]y", colors(), invert = TRUE)], 1)),
                    
                    # Give this color div an id so it can be removed easily
                    id = div_id
                )
            )
            
            # Render the filter header text
            output[[header_id]] <-
                renderUI(h4(paste("Color", n)))
            
            # Add one to the color counter
            num_colors(num_colors() + 1)
        })
        
        # Update the choices of terms to color nodes by based on selected column
        observe({
            if (num_colors() > prev_num_colors()) {
                for (n in 1:num_colors()) {
                    # Get the ids of the widgets for this div
                    column_id <- paste("color_column", n, sep = "_")
                    values_id <- paste("color_values", n, sep = "_")
                    
                    # Update the dropdown for selecting the values
                    observeEvent(input[[column_id]], {
                        updateSelectizeInput(
                            session,
                            values_id,
                            choices = sort(unique(
                                vertex_attr(dat_igraph(), input[[column_id]])
                            ))
                        )
                    })
                }
            }
            
            # Set the previous number of colors to the current number
            prev_num_colors(num_colors())
        })
        
        # Remove the last color when the remove color button is clicked
        observeEvent(input$remove_color, {
            # Clear the input values or the app won't actually update
            updateSelectizeInput(
                session,
                paste("network_color_values", num_colors(), sep = "_"),
                choices = character(0),
                selected = character(0)
            )
            
            # Remove color div
            removeUI(paste('#color_div', num_colors(), sep = "_"))
            
            # Subtract one from the color counter
            num_colors(num_colors() - 1)
        })
        
        # Convert the selected node colors into a data frame
        node_colors <- reactive({
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
            node_color_df <- separate_rows(node_color_df, 2, sep = ",")
            
            # Return the completed data frame
            return(node_color_df)
        })
        
        # Generate the network plot
        network_plot <- reactive({
            # Check if dat_igraph is an igraph object
            if (!is.igraph(dat_igraph())) {
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


## DOWNLOAD NETWORK ------------------------------------------------------------


# The UI for the network tab's download section is defined in "reused_modules.R"

# Define the server for the network tab's download section
download_network_server <- function(id, network_plot) {
    moduleServer(id, function(input, output, session) {
        observeEvent(network_plot(), {
            # Decide on a pdf size based on the number of nodes
            plot_size <- max(nrow(network_plot()$data) / 3, 10)
            
            # Update the pdf width
            updateTextInput(
                session,
                "width",
                value = plot_size
            )
            
            # Update the pdf height
            updateTextInput(
                session,
                "height",
                value = plot_size
            )
            
            # Handle downloading the network as a pdf
            output$download <-
                downloadHandler(
                    "network.pdf",
                    function(file) {
                        # Save the pdf
                        ggsave(
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

