## File containing the module UI and server for the dataset selection tab


# DEFINE UI --------------------------------------------------------------------


selectDatasetTabUI <- function(id) {
    # Create a page with a sidebar layout
    shiny::fluidPage(
        shiny::sidebarLayout(

            # Create the sidebar
            shiny::sidebarPanel(
                # Add a dropdown menu to select a dataset
                selectDatasetUI(NS(id, "selectDataset")),

                # Add a section to filter the dataset
                filterDatasetUI(NS(id, "filterDataset"))
            ),

            # Create the main panel
            shiny::mainPanel(
                # Add a display of the selected, filtered dataset
                displayDatasetUI(NS(id, "displayDataset"))
            )
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


selectDatasetTabServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        # Get the selected dataset
        dataset <-
            selectDatasetServer("selectDataset")

        # Apply filters to the selected dataset
        filteredDataset <-
            filterDatasetServer("filterDataset", dataset)

        # Display the filtered dataset
        displayDatasetServer("displayDataset", filteredDataset)

        # Return the filtered dataset
        return(filteredDataset)
    })
}

