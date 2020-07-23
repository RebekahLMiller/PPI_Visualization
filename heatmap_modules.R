## File containing the shiny modules unique to the heatmap tab


## GENERATE HEATMAP ------------------------------------------------------------


# Define the UI for the heatmap tab's plot generation
generate_heatmap_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    tagList(
        # Select the variable to plot as columns
        selectInput(ns("select_column"),
                    "Select the variable to plot on the x-axis",
                    choices = NULL),
        
        # Select the variable to plot as rows
        selectInput(ns("select_row"),
                    "Select the variable to plot on the y-axis",
                    choices = NULL),
        
        # Add a title to the heatmap
        textInput(ns("title"),
                  "Add a title"),
        
        # Add an x-axis label
        textInput(ns("xlab"),
                  "Add a label to the x-axis"),
        
        # Add a y-axis label
        textInput(ns("ylab"),
                  "Add a label to the y-axis"),
        
        # Choose the low end of the color gradient
        colourInput(ns("low_color"),
                    "Pick a color for the low end of the gradient",
                    value = "lightgray"),
        
        # Choose the high end of the color gradient
        colourInput(ns("high_color"),
                    "Pick a color for the high end of the gradient",
                    value = "midnightblue"),
        
        # Choose whether or not to display the legend
        checkboxInput(ns("show_legend"),
                      "Should the legend be plotted?",
                      value = TRUE),
        
        # Show an additional panel if the legend should be displayed
        conditionalPanel(
            "input.show_legend == 1",
            
            # Set the namespace
            ns = ns,
            
            # Add a label to the legend
            textInput(ns("legend_title"),
                      "Add a label to the legend")
        )
    )
}

# Define the server for the heatmap tab's plot generation
generate_heatmap_server <- function(id, dat, filters) {
    moduleServer(id, function(input, output, session) {
        # Update heatmap row and column choices based on selected dataset
        observeEvent(dat(), {
            # Update choices for variables to use as rows
            updateSelectInput(
                session,
                "select_row",
                choices = colnames(dat())
            )
            
            # Update choices for variables to use as columns
            updateSelectInput(
                session,
                "select_column",
                choices = colnames(dat())
            )
        })
        
        # Generate the heatmap
        heatmap_plot <- reactive({
            # Make sure the selected column and row are columns in dat()
            req(input$select_column %in% colnames(dat()))
            req(input$select_row %in% colnames(dat()))
            
            # Generate the plot
            heatmap_plot <- plot_heatmap(
                dat(),
                input$select_column,
                input$select_row,
                filters(),
                input$title,
                input$xlab,
                input$ylab,
                input$show_legend,
                input$legend_title,
                input$low_color,
                input$high_color
            )
            
            # Return the heatmap
            return(heatmap_plot)
        })
        
        # Return the heatmap
        return(heatmap_plot)
    })
}


## DOWNLOAD HEATMAP ------------------------------------------------------------


# The UI for the heatmap tab's download section is defined in "reused_modules.R"

# Define the server for the heatmap tab's download section
download_heatmap_server <- function(id, heatmap_plot) {
    moduleServer(id, function(input, output, session) {
        observeEvent(heatmap_plot(), {
            # Update the pdf width based on the number of columns in the heatmap
            updateTextInput(
                session,
                "width",
                value = max(length(unique(heatmap_plot()$data$Group.1)) / 2, 10)
            )
            
            # Update the pdf height based on the number of rows in the heatmap
            updateTextInput(
                session,
                "height",
                value = max(length(unique(heatmap_plot()$data$Group.2)) / 2, 10)
            )
        })
        
        # Handle downloading the heatmap as a pdf
        output$download <-
            downloadHandler(
                "heatmap.pdf",
                function(file) {
                    # Save the pdf
                    ggsave(
                        file,
                        plot = heatmap_plot(),
                        device = "pdf",
                        width = as.numeric(input$width),
                        height = as.numeric(input$height),
                        units = "cm",
                        limitsize = FALSE
                    )
                }
            )
    })
}

