## File containing the module UI and server for the heatmap tab


# DEFINE UI --------------------------------------------------------------------


heatmapTabUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a page with a sidebar layout
    shiny::fluidPage(
        shiny::sidebarLayout(

            # Create the sidebar
            shiny::sidebarPanel(
                # Add options for customizing the heatmap
                makeHeatmapUI(ns("makeHeatmap"))
            ),

            # Create the main panel
            shiny::mainPanel(
                # Plot the heatmap
                displayPlotUI(ns("displayHeatmap")),

                # Set up and download the heatmap as a pdf
                downloadPlotUI(ns("downloadHeatmap"))
            )
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


heatmapTabServer <- function(id, interactionCounts) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure interactionCounts is a reactive
        stopifnot(shiny::is.reactive(interactionCounts))

        # Create the heatmap
        heatmapPlot <-
            makeHeatmapServer("makeHeatmap", interactionCounts)

        # Display the heatmap
        displayPlotServer("displayHeatmap", heatmapPlot)

        # Download the heatmap
        downloadPlotServer("downloadHeatmap", heatmapPlot, "heatmap.pdf")
    })
}

