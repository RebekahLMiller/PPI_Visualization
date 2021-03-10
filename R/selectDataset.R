## File containing the module UI and server for selecting a dataset


# DEFINE UI --------------------------------------------------------------------


selectDatasetUI <- function(id) {
    # Create a dropdown menu to select a dataset
    shiny::selectInput(
        NS(id, "select"),
        label = "Select a dataset",
        choices = c(
            "Human APID (old)",
            "Human APID",
            "Human BioGRID",
            "COFpendium"
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
                "Human APID (old)" = human_apid_old,
                "Human APID" = human_apid,
                "Human BioGRID" = human_biogrid,
                "COFpendium" = cofpendium
            )
        })
    })
}

