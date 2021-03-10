## File containing the module UI and server for filtering a dataset


# DEFINE UI --------------------------------------------------------------------


filterDatasetUI <- function(id) {
    shiny::tagList(
        # Create a checkbox to determine whether the filters are displayed
        shiny::checkboxInput(
            NS(id, "displayFilters"),
            label = "Display filtering options?"
        ),

        # Only show this panel if the displayFilters checkbox is checked
        conditionalPanel(
            condition = "input.displayFilters == 1",

            # Create a series of checkboxes to select the columns to filter by
            shiny::checkboxGroupInput(
                NS(id, "selectColumns"),
                label = "Select the columns to filter by",
                choices = NULL,
                selected = NULL
            ),

            # Set the namespace
            ns = NS(id)
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


filterDatasetServer <- function(id, dataset) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure dataset is a reactive
        stopifnot(shiny::is.reactive(dataset))

        # Make a reactive list to keep track of which columns are selected
        selectedColumns <-
            reactiveValues(selected = NULL, toAdd = NULL, toRemove = NULL)

        # Update column name checkboxes when the selected dataset changes
        shiny::observeEvent(dataset(), {
            shiny::updateCheckboxGroupInput(
                session,
                "selectColumns",
                choices = colnames(dataset())
            )
        })

        # Update the list of selected columns when the selected columns change
        shiny::observeEvent(input$selectColumns, {
            # Figure out which columns need to have a filter section added
            selectedColumns$toAdd <-
                dplyr::setdiff(input$selectColumns, selectedColumns$selected)

            # Figure out which columns need to have a filter section removed
            selectedColumns$toRemove <-
                dplyr::setdiff(selectedColumns$selected, input$selectColumns)

        }, ignoreNULL = FALSE)

        # Add a filter section for each newly selected column
        shiny::observeEvent(selectedColumns$toAdd, {
            for (addColumn in selectedColumns$toAdd) {
                addFilter(id, addColumn, selectedColumns, dataset)
            }
        })

        # Remove the filter section for each newly unselected column
        shiny::observeEvent(selectedColumns$toRemove, {
            for (removeColumn in selectedColumns$toRemove) {
                removeFilter(id, removeColumn, selectedColumns)
            }
        })

        # Filter the dataset based on the currently active filter columns
        filteredDataset <- shiny::reactive({
            return(filterDataset(input, dataset(), selectedColumns))
        })

        # Return the filtered dataset
        return(filteredDataset)
    })
}


# DEFINE HELPER FUNCTIONS ------------------------------------------------------


# Helper function to add a filter section when a column checkbox is clicked
# Inputs:
#   id: the module ID string to be namespaced
#   addColumn: the name of the column to add a filter section for
#   selectedColumns: the reactive list tracking which columns are selected
#   dataset: the reactive dataset to filter
addFilter <- function(id, addColumn, selectedColumns, dataset) {
    # Insert a new UI element to select terms to filter
    shiny::insertUI(
        selector = paste0("#", NS(id, "selectColumns")),
        where = "afterEnd",
        ui = selectizeInput(
            NS(id, paste0(addColumn, "Filter")),
            label = paste("Select the values to filter by in", addColumn),
            choices = sort(unique(dataset()[, addColumn])),
            multiple = TRUE
        )
    )

    # Move the column name from toAdd to selected
    selectedColumns$toAdd <-
        selectedColumns$toAdd[selectedColumns$toAdd != addColumn]
    selectedColumns$selected <-
        append(selectedColumns$selected, addColumn)
}

# Helper function to remove a filter section when a column checkbox is unclicked
# Inputs:
#   id: the module ID string to be namespaced
#   removeColumn: the name of the column to remove the filter section for
#   selectedColumns: the reactive list tracking which columns are selected
removeFilter <- function(id, removeColumn, selectedColumns) {
    # Remove the UI element to select terms to filter
    shiny::removeUI(
        paste0("div:has(>> #", NS(id, paste0(removeColumn, "Filter")), ")")
    )

    # Remove the column name from toRemove and selected
    selectedColumns$toRemove <-
        selectedColumns$toRemove[selectedColumns$toRemove != removeColumn]
    selectedColumns$selected <-
        selectedColumns$selected[selectedColumns$selected != removeColumn]
}

# Helper function to apply the selected filters to the selected dataset
# Inputs:
#   input: the user inputs
#   dataset: the selected, unfiltered dataset
#   selectedColumns: the reactive list tracking which columns are selected
filterDataset <- function(input, dataset, selectedColumns) {
    # Start with the unfiltered dataset
    filteredDataset <- dataset

    # Loop through the columns to filter the dataset by
    for (filterColumn in selectedColumns$selected) {
        # Skip filtering by this column if it's not in the dataset
        # (This will happen right after the selected dataset changes)
        if (!(filterColumn %in% colnames(filteredDataset))) {
            next()
        }

        # Get the list of values to look for in this column
        filterValues <- input[[paste0(filterColumn, "Filter")]]

        # If no values have been selected yet, skip this column
        if (is.null(filterValues)) {
            next()
        }

        # Filter the dataset by the values selected for this column
        filteredDataset <-
            filteredDataset %>%
            dplyr::filter(!!as.symbol(filterColumn) %in% filterValues)
    }

    # Return the filtered dataset
    return(filteredDataset)
}

