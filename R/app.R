## Script for running interactive PPI visualization app


# SET UP -----------------------------------------------------------------------


# Define the tidyverse pipe (%>%) so it can be used without attaching packages
`%>%` <- magrittr::`%>%`


# DEFINE UI --------------------------------------------------------------------

runPPIApp <- function() {
ui <- shiny::navbarPage(
    "PPI Visualization",

    # Make a tab to select and filter the dataset
    shiny::tabPanel(
        "Select and Filter Data",

        shiny::fluidPage(
            # Allow enabling/disabling of widgets
            shinyjs::useShinyjs(),

            shiny::sidebarLayout(
                # Make a sidebar to select dataset and add filters
                shiny::sidebarPanel(
                    # Select a dataset
                    selectDatasetUI("select_data"),

                    # Add filters
                    filterDatasetUI("filter_data")
                ),

                # Make a main panel to display the selected, filtered dataset
                shiny::mainPanel(
                    displayDatasetUI("display_data")
                )
            )
        )
    ),

    # Make a tab to generate heatmaps
    shiny::tabPanel(
        "Generate Heatmap",

        shiny::fluidPage(
            shiny::sidebarLayout(
                # Make a sidebar to generate a heatmap with specific settings
                shiny::sidebarPanel(
                    generate_heatmap_UI("generate_heatmap")
                ),

                # Make a main panel to display the heatmap
                shiny::mainPanel(shiny::tabsetPanel(
                    # Make a tab to display the heatmap in plot format
                    shiny::tabPanel(
                        "Plot",

                        # Plot the heatmap
                        display_plot_UI("display_heatmap"),

                        # Set up and download the heatmap as a pdf
                        download_plot_UI("download_heatmap")
                    ),

                    # Make a tab to display the heatmap as a numeric table
                    shiny::tabPanel(
                        "Table",

                        # Download the numeric table used to make the heatmap
                        download_table_UI("download_table"))
                ))
            )
        )
    ),

    # Make a tab to generate network graphs
    shiny::tabPanel(
        "Generate Network",

        shiny::fluidPage(
            shiny::sidebarLayout(
                # Make a sidebar to generate a network with specific settings
                shiny::sidebarPanel(
                    generate_network_UI("generate_network")
                ),

                # Make a main panel to display the heatmap
                shiny::mainPanel(
                    # Plot the heatmap
                    display_plot_UI("display_network"),

                    # Set up and download the heatmap as a pdf
                    download_plot_UI("download_network")
                )
            )
        )
    )
)


# DEFINE SERVER ----------------------------------------------------------------


server <- function(input, output, session) {
    # Determine which dataset is selected
    dat <- selectDatasetServer("select_data")

    # Get a list of all the filters to apply to the dataset
    filters <- filterDatasetServer("filter_data", shiny::reactive(dat()))

    # Filter the dataset
    filtered_dat <- shiny::reactive({
        return(filter_rows(dat(), filters()))
    })

    # Display the filtered dataset
    displayDatasetServer("display_data", shiny::reactive(filtered_dat()))

    # Generate the heatmap
    heatmap_plot <-
        generate_heatmap_server("generate_heatmap",
                                shiny::reactive(dat()),
                                shiny::reactive(filters()))

    # Display the heatmap
    display_plot_server("display_heatmap", shiny::reactive(heatmap_plot()))

    # Download the heatmap
    download_heatmap_server("download_heatmap", shiny::reactive(heatmap_plot()))

    # Download the numeric table used to make the heatmap
    download_table_server("download_table", shiny::reactive(heatmap_plot()))

    # Make an igraph object for plotting the network
    dat_igraph <- shiny::reactive({
        extract_igraph(filtered_dat())
    })

    # Generate the network plot
    network_plot <-
        generate_network_server("generate_network",
                                shiny::reactive(dat_igraph()))

    # Display the network plot
    display_plot_server("display_network", shiny::reactive(network_plot()))

    # Download the network plot
    download_network_server("download_network", shiny::reactive(network_plot()))
}


# RUN APP ----------------------------------------------------------------------


shiny::shinyApp(ui, server)
}


runTestApp <- function() {
    # Create a page to select, filter, and display a dataset
    selectTabUI <- shiny::fluidPage(
        shiny::sidebarLayout(

            # Create the sidebar
            shiny::sidebarPanel(
                # Add a dropdown menu to select a dataset
                selectDatasetUI("selectDataset"),

                # Add buttons to filter the dataset
                filterDatasetUI("filterDataset")
            ),

            # Create the main panel
            shiny::mainPanel(
                # Add a display of the selected, filtered dataset
                displayDatasetUI("displayDataset")
            )
        )
    )

    selectTabServer <- function(input, output, session) {
        # Get the selected dataset
        dataset <-
            selectDatasetServer("selectDataset")

        # Apply filters to the selected dataset
        filteredDataset <-
            filterDatasetServer(
                "filterDataset",
                shiny::reactive(dataset())
            )

        # Display the filtered dataset
        displayDatasetServer(
            "displayDataset",
            shiny::reactive(filteredDataset())
        )
    }

    # Run the app
    shiny::shinyApp(selectTabUI, selectTabServer)
}

