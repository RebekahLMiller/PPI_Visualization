## File containing the shiny modules unique to the select data tab


## SELECT DATASET --------------------------------------------------------------


# Define the UI for the select data tab's data selection
select_data_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    # Select a dataset
    selectInput(
        ns("select"),
        "Select a dataset",
        choices = c("Human APID", "Human HuRI", "Mouse APID")
    )
}

# Define the server for the select data tab's data selection
select_data_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Determine which dataset is selected
        dat <- reactive({
            switch(
                input$select,
                "Human APID" = human_apid,
                "Human HuRI" = human_huri,
                "Mouse APID" = mouse_apid
            )
        })
        
        # Return the selected dataset
        return(dat)
    })
}


## FILTER DATASET --------------------------------------------------------------


# Define the UI for the select data tab's filters
filter_data_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    tagList(
        # Placeholder for filters
        tags$div(id = ns("filters_placeholder")),
        
        # Button to add filters
        actionButton(ns("add_filter"), "Add a filter"),
        
        # Button to remove filters
        actionButton(ns("remove_filter"), "Remove a filter")
    )
}

# Define the server for the select data tab's filters
filter_data_server <- function(id, dat) {
    moduleServer(id, function(input, output, session) {
        # Set the namespace
        ns = session$ns
        
        # Keep track of the current and maximum number of filters
        num_filters <- reactiveVal(0)
        max_num_filters <- reactiveVal(0)
        
        # Disable the remove filter button if there are no filters
        observe({
            if (num_filters() == 0) {
                disable("remove_filter")
            } else {
                enable("remove_filter")
            }
        })
        
        # Add a filter when the add filter button is clicked
        observeEvent(input$add_filter, {
            # Make ids for referencing the new filter div and its contents
            n <- num_filters() + 1
            div_id <- paste("filter_div", n, sep = "_")
            column_id <- paste("filter_column", n, sep = "_")
            values_id <- paste("filter_values", n, sep = "_")
            
            # Add filter widgets
            insertUI(
                # Set the location for the new widgets
                paste0("#", ns("filters_placeholder")),
                "beforeEnd",
                
                # Define the UI for the new widgets
                tags$div(
                    # Add filter header
                    h4(paste("Filter", n)),
                    
                    # Add dropdown menu to choose column to filter by
                    selectInput(
                        ns(column_id),
                        "Select a variable to filter by",
                        colnames(dat())
                    ),
                    
                    # Add dropdown menu to select terms to filter
                    selectizeInput(
                        ns(values_id),
                        "Select the values to search for",
                        choices = NULL,
                        multiple = TRUE
                    ),
                    
                    # Give this filter div an id so it can be removed easily
                    id = div_id
                ),
                
                # Insert the new widgets immediately
                immediate = TRUE
            )
            
            # Only create a new observeEvent the first time filter n is created
            if (n > max_num_filters()) {
                # Update the available values when the selected column changes
                observeEvent(input[[column_id]], {
                    # Get the selected column name
                    column_name <- input[[column_id]]
                    
                    # Make sure it's a valid column name
                    if (column_name %in% colnames(dat())) {
                        # Get the values from the selected column
                        choices <- sort(unique(dat()[, column_name]))
                        
                        # Split semicolon delimited character values
                        if (is.character(choices)) {
                            choices <-
                                sort(unique(unlist(strsplit(choices, ";"))))
                        }
                        
                        # Update the dropdown menu
                        updateSelectizeInput(
                            session,
                            values_id,
                            choices = choices
                        )
                    }
                })
            }
            
            # Add one to the filter counter
            num_filters(num_filters() + 1)
            
            # Update the maximum number of filters if necessary
            if (num_filters() > max_num_filters()) {
                max_num_filters(num_filters())
            }
        })
        
        # Define a function to remove the last filter
        remove_filter <- function() {
            # Clear the column selection menu to force its observeEvent() to run
            updateSelectInput(
                session,
                paste("filter_column", num_filters(), sep = "_"),
                choices = " ",
                selected = " "
            )
            
            # Remove filter div
            removeUI(paste('#filter_div', num_filters(), sep = "_"))
            
            # Subtract one from the filter counter
            num_filters(num_filters() - 1)
        }
        
        # Remove the last filter when the remove filter button is clicked
        observeEvent(input$remove_filter, {
            remove_filter()
        })
        
        # Remove all filters if dat changes
        observeEvent(dat(), {
            while (num_filters() > 0) {
                remove_filter()
            }
        })
        
        # Compile all the filters into a named list
        filters <- reactive({
            # Initialize an empty list
            filters_list <- list()
            
            # Add elements to the list
            lapply(1:num_filters(), function(x) {
                # Figure out which column is being used to filter
                filter_column <-
                    input[[paste("filter_column", x, sep = "_")]]
                
                # Figure out the values to search for in this column
                filter_values <-
                    input[[paste("filter_values", x, sep = "_")]]
                
                # Add element to the list where name = column, value = values
                if (length(filter_values > 0)) {
                    filters_list[[filter_column]] <<- filter_values
                }
            })
            
            # Return complete list
            return(filters_list)
        })
        
        # Return the filter list
        return(filters)
    })
}


## DISPLAY DATASET -------------------------------------------------------------


# Define the UI for the select data tab's data display
display_data_UI <- function(id) {
    # Set the namespace
    ns <- NS(id)
    
    # Display the selected, filtered dataset
    dataTableOutput(ns("display_dataset"))
}

# Define the UI for the select data tab's data display
display_data_server <- function(id, filtered_dat) {
    moduleServer(id, function(input, output, session) {
        # Display the filtered dataset
        output$display_dataset <-
            renderDataTable(
                filtered_dat(),
                # Add a scrollbar if the table is too wide (it is)
                options = list(scrollX = TRUE)
            )
    })
}

