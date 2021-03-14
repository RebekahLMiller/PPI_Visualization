## File containing the module UI and server for displaying a plot


# DEFINE UI --------------------------------------------------------------------


displayPlotUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a plot output to display the plot
    shiny::plotOutput(ns("displayPlot"))
}


# DEFINE SERVER ----------------------------------------------------------------


displayPlotServer <- function(id, displayPlot) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure displayPlot is a reactive
        stopifnot(shiny::is.reactive(displayPlot))

        # Display the plot
        output$displayPlot <-
            renderPlot(displayPlot())
    })
}

