## File containing the module UI and server for displaying a dataset


# DEFINE UI --------------------------------------------------------------------


displayTableUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a data table output to display a table
    DT::dataTableOutput(ns("displayTable"))
}


# DEFINE SERVER ----------------------------------------------------------------


displayTableServer <- function(id, displayTable) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure displayTable is a reactive
        stopifnot(shiny::is.reactive(displayTable))

        # Display the table
        output$displayTable <-
            DT::renderDataTable(
                displayTable(),
                # Add a scrollbar if the table is too wide
                options = list(scrollX = TRUE)
            )
    })
}

