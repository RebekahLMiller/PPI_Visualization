## File containing the shiny modules used in more than one tab


## DISPLAY PLOT ----------------------------------------------------------------


# Define the UI for the plot's display
display_plot_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    # Display the plot
    plotOutput(ns("display"))
}

# Define the server for the plot's display
display_plot_server <- function(id, plt) {
    moduleServer(id, function(input, output, session) {
        # Display the plot
        output$display <- renderPlot(plt())
    })
}


## DOWNLOAD PLOT ---------------------------------------------------------------


# Define the UI for the plot's download section
download_plot_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    # Set up and download the heatmap as a pdf
    fluidRow(
        # Text input to set pdf height
        column(4, textInput(ns("height"),
                            "Set PDF height (cm)")), 
        
        # Text input to set pdf width
        column(4, textInput(ns("width"),
                            "Set PDF width (cm)")), 
        
        # Button to download plot as a pdf
        column(2, downloadButton(ns("download")),
               # Shift the download button down so it's aligned better
               style = "margin-top: 25px;")
    )
}

# The servers for downloading plots are defined in separate files

