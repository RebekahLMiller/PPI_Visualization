## File containing the module UI and server for selecting a dataset


# DEFINE UI --------------------------------------------------------------------


selectDatasetUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a dropdown menu to select a dataset
    shiny::selectInput(
        ns("select"),
        label = "Select a dataset",
        choices = c(
            "Human APID",
            "Human BioGRID",
            "Test List"
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


selectDatasetServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        # Return the selected dataset as a reactive
        shiny::reactive({
            switch(
                input$select,
                "Human APID" = human_apid,
                "Human BioGRID" = human_biogrid,
                "Test List" = testPPIList
            )
        })
    })
}

