## File containing the shiny modules unique to the heatmap tab


# GENERATE HEATMAP -------------------------------------------------------------


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
        
        # Select the variable to use to color the heatmap
        selectInput(ns("select_fill"),
                    "Select the variable to use to color the heatmap",
                    choices = NULL),
        
        # Choose whether or not to cluster the columns
        checkboxInput(ns("cluster_columns"),
                      "Cluster columns",
                      value = FALSE),
        
        # Choose whether or not to cluster the rows
        checkboxInput(ns("cluster_rows"),
                      "Cluster rows",
                      value = FALSE),
        
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
        # Update heatmap row, column, and fill choices based on selected dataset
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
            
            # Update choices for variables to use to color the heatmap
            updateSelectInput(
                session,
                "select_fill",
                choices = c("Number of interactions",
                            names(which(map_lgl(dat(), is.numeric))))
            )
        })
        
        # Generate the heatmap
        heatmap_plot <- reactive({
            # Make sure the selected column and row are columns in dat()
            req(input$select_column %in% colnames(dat()))
            req(input$select_row %in% colnames(dat()))
            
            # Make sure input$select_fill is the right format for plot_heatmap()
            select_fill <- input$select_fill
            if (select_fill == "Number of interactions") {
                select_fill <- NA
            }
            
            # Generate the plot
            heatmap_plot <- plot_heatmap(
                dat(),
                input$select_column,
                input$select_row,
                select_fill,
                filters(),
                input$cluster_columns,
                input$cluster_rows,
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


# DOWNLOAD HEATMAP -------------------------------------------------------------


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


# DOWNLOAD TABLE ---------------------------------------------------------------


# Define the UI for the numeric heatmap table download section
download_table_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    # Download the table as a tsv file
    tagList(
        # Radio button to toggle between wide and long format
        radioButtons(
            ns("toggle_format"),
            "Toggle table format",
            choices = c("Long", "Wide"),
            inline = TRUE
        ),
        
        # Display a message saying the displayed table is just a preview
        textOutput(ns("preview_text")),
        
        # Insert some space between the message and the table
        br(),
        
        # Display the table
        dataTableOutput(ns("display_table")),
        
        # Button to download the table
        downloadButton(ns("download_table"))
    )
}

# Define the server for the heatmap tab's download section
download_table_server <- function(id, heatmap_plot) {
    moduleServer(id, function(input, output, session) {
        # Get the column names of the table to convert it to wide format
        col_names <- reactive({colnames(heatmap_plot()$data)})
        
        # Convert the table to wide format
        wide_table <- reactive({
            # Convert to wide format
            wide_table <- dcast(
                heatmap_plot()$data,
                get(col_names()[2]) ~ get(col_names()[1]),
                value.var = col_names()[3]
            )
            
            # Fix the column names
            names(wide_table)[1] <- col_names()[2]
            
            # Return the wide format table
            return(wide_table)
        })
        
        # Toggle displayed table format based on radio button input
        heatmap_table <- reactive({
            switch(
                input$toggle_format,
                "Long" = heatmap_plot()$data,
                "Wide" = wide_table()
            )
        })
        
        # Display a message saying the displayed table is just a preview
        output$preview_text <-
            renderText(
                paste(
                    "Note: Only the first 100 columns are displayed in wide",
                    "format to reduce rendering time. If you need the whole",
                    "table, download it using the button below the table."
                )
            )
        
        # Display the table
        output$display_table <-
            renderDataTable(
                # Display a maximum of 100 columns so it doesn't take forever
                heatmap_table()[, 1:min(100, ncol(heatmap_table()))],
                # Add a scrollbar if the table is too wide
                options = list(scrollX = TRUE)
            )
        
        # Handle downloading the heatmap table as a tsv file
        output$download_table <-
            downloadHandler(
                "heatmap_table.tsv",
                function(file) {
                    # Save the table as a tsv file
                    write.table(
                        heatmap_table(),
                        file,
                        quote = FALSE,
                        row.names = FALSE,
                        sep = "\t"
                    )
                }
            )
    })
}

