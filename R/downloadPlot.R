## File containing the module UI and server for downloading a plot


# DEFINE UI --------------------------------------------------------------------


downloadPlotUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    shiny::fluidRow(
        # Create a numeric input box to set the pdf height
        shiny::column(
            4,
            shiny::numericInput(
                ns("height"),
                label = "Set PDF height (cm)",
                value = 10
            )
        ),

        # Create a text input box to set the pdf width
        shiny::column(
            4,
            shiny::numericInput(
                ns("width"),
                label = "Set PDF width (cm)",
                value = 10
            )
        ),

        # Create a button to download the plot as a pdf
        shiny::column(
            2,
            shiny::downloadButton(
                ns("download")
            ),
            # Shift the download button down so it's aligned better
            style = "margin-top: 25px;"
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


downloadPlotServer <- function(id, downloadPlot, fileName) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure downloadPlot is a reactive and fileName is not
        stopifnot(shiny::is.reactive(downloadPlot))
        stopifnot(!shiny::is.reactive(fileName))

        # Download the plot as a pdf when the download button is clicked
        output$download <-
            shiny::downloadHandler(
                fileName,
                function(file) {
                    # Save the pdf
                    ggplot2::ggsave(
                        file,
                        plot = downloadPlot(),
                        device = "pdf",
                        width = input$width,
                        height = input$height,
                        units = "cm",
                        limitsize = FALSE
                    )
                }
            )
    })
}

