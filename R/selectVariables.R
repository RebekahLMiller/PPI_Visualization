## File containing the module UI and server for counting interactions


# DEFINE UI --------------------------------------------------------------------


selectVariablesUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    shiny::tagList(
        # Create a dropdown menu to select the first variable
        shiny::selectInput(
            ns("selectVariable1"),
            label = "Select the first variable",
            choices = NULL
        ),

        # Create a dropdown menu to select the second variable
        shiny::selectInput(
            ns("selectVariable2"),
            label = "Select the second variable",
            choices = NULL
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


selectVariablesServer <- function(id, filteredDataset) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure filteredDataset is a reactive
        stopifnot(shiny::is.reactive(filteredDataset))

        # Update dropdown menu choices when the filtered dataset changes
        shiny::observeEvent(filteredDataset(), {
            # Update the choices for the first variable
            shiny::updateSelectInput(
                session,
                "selectVariable1",
                choices = colnames(filteredDataset())
            )

            # Update the choices for the second variable
            shiny::updateSelectInput(
                session,
                "selectVariable2",
                choices = colnames(filteredDataset())
            )
        })

        # Get the number of unique IDs per combinaton of selected variables
        interactionCounts <-
            shiny::reactive(
                filteredDataset() %>%

                    # Extract the selected columns
                    dplyr::select(
                        input$selectVariable1,
                        input$selectVariable2,
                        "Interaction_ID"
                    ) %>%

                    # Remove duplicate rows
                    dplyr::distinct() %>%

                    # Count the number of unique IDs for each combination
                    dplyr::group_by(
                        !!as.symbol(input$selectVariable1),
                        !!as.symbol(input$selectVariable2)
                    ) %>%
                    dplyr::count(name = "number_unique_interaction_IDs")
            )

        # Return the interaction counts table
        return(interactionCounts)
    })
}

