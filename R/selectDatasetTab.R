## File containing the module UI and server for the dataset selection tab


# DEFINE UI --------------------------------------------------------------------


selectDatasetTabUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a page with a sidebar layout
    shiny::fluidPage(
        shiny::sidebarLayout(

            # Create the sidebar
            shiny::sidebarPanel(
                # Add a dropdown menu to select a dataset
                selectDatasetUI(ns("selectDataset")),

                # Add a section to filter the dataset
                filterDatasetUI(ns("filterDataset"))
            ),

            # Create the main panel
            shiny::mainPanel(
                # Add a display of the selected, filtered dataset
                displayTableUI(ns("displayDataset")),

                # Download the table as a text file
                downloadTableUI(ns("downloadDataset"))
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
        displayTableServer("displayDataset", filteredDataset)

        # Download the filtered dataset
        downloadTableServer(
            "downloadDataset",
            filteredDataset,
            "filtered_PPI_list.tsv"
        )

        # Return the filtered dataset
        return(filteredDataset)
    })
}

