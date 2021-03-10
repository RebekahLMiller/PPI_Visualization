## File containing the module UI and server for displaying a dataset


# DEFINE UI --------------------------------------------------------------------


displayDatasetUI <- function(id) {
    # Create a data table output to display the selected, filtered dataset
    DT::dataTableOutput(shiny::NS(id, "displayDataset"))
}


# DEFINE SERVER ----------------------------------------------------------------


displayDatasetServer <- function(id, filteredDataset) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure filteredDataset is a reactive
        stopifnot(shiny::is.reactive(filteredDataset))

        # Display the filtered dataset
        output$displayDataset <-
            DT::renderDataTable(
                filteredDataset(),
                # Add a scrollbar if the table is too wide
                options = list(scrollX = TRUE)
            )
    })
}

