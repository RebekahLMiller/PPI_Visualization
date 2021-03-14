## File containing the module UI and server for the variable selection tab


# DEFINE UI --------------------------------------------------------------------


selectVariablesTabUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    # Create a page with a sidebar layout
    shiny::fluidPage(
        shiny::sidebarLayout(

            # Create the sidebar
            shiny::sidebarPanel(
                # Add dropdown menus to select which columns to use
                selectVariablesUI(ns("selectVariables"))
            ),

            # Create the main panel
            shiny::mainPanel(
                # Display the table of interactions for the selected variables
                displayTableUI(ns("displayInteractionCounts")),

                # Download the table as a text file
                downloadTableUI(ns("downloadInteractionCounts"))
            )
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


selectVariablesTabServer <- function(id, filteredDataset) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure filteredDataset is a reactive
        stopifnot(shiny::is.reactive(filteredDataset))

        # Reformat the data based on the selected columns
        interactionCounts <-
            selectVariablesServer("selectVariables", filteredDataset)

        # Display the table of interactions for the selected variables
        displayTableServer("displayInteractionCounts", interactionCounts)

        # Download the table of interaction counts for the selected variables
        downloadTableServer(
            "downloadInteractionCounts",
            interactionCounts,
            "selected_variables_interaction_counts.tsv"
        )

        # Return the table of interaction counts for the selected variables
        return(interactionCounts)
    })
}

