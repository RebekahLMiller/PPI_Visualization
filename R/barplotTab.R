## File containing the module UI and server for the barplot tab


# DEFINE UI --------------------------------------------------------------------


barplotTabUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a page with a sidebar layout
    shiny::fluidPage(
        shiny::sidebarLayout(

            # Create the sidebar
            shiny::sidebarPanel(
                # Add options for customizing the barplot
                makeBarplotUI(ns("makeBarplot"))
            ),

            # Create the main panel
            shiny::mainPanel(
                # Plot the barplot
                displayPlotUI(ns("displayBarplot")),

                # Set up and download the barplot as a pdf
                downloadPlotUI(ns("downloadBarplot"))
            )
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


barplotTabServer <- function(id, interactionCounts) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure interactionCounts is a reactive
        stopifnot(shiny::is.reactive(interactionCounts))

        # Create the barplot
        barplotPlot <-
            makeBarplotServer("makeBarplot", interactionCounts)

        # Display the barplot
        displayPlotServer("displayBarplot", barplotPlot)

        # Download the barplot
        downloadPlotServer("downloadBarplot", barplotPlot, "barplot.pdf")
    })
}

