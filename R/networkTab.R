## File containing the module UI and server for the network plot tab


# DEFINE UI --------------------------------------------------------------------


networkTabUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a page with a sidebar layout
    shiny::fluidPage(
        shiny::sidebarLayout(

            # Create the sidebar
            shiny::sidebarPanel(
                # Add options for customizing the network plot
                makeNetworkUI(ns("makeNetwork"))
            ),

            # Create the main panel
            shiny::mainPanel(
                # Plot the network
                displayPlotUI(ns("displayNetwork")),

                # Set up and download the network plot as a pdf
                downloadPlotUI(ns("downloadNetwork"))
            )
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


networkTabServer <- function(id, filteredDataset) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure filteredDataset is a reactive
        stopifnot(shiny::is.reactive(filteredDataset))

        # Create the network plot
        networkPlot <-
            makeNetworkServer("makeNetwork", filteredDataset)

        # Display the network plot
        displayPlotServer("displayNetwork", networkPlot)

        # Download the network plot
        downloadPlotServer("downloadNetwork", networkPlot, "network.pdf")
    })
}

