## Script for running interactive PPI visualization app


# SET UP -----------------------------------------------------------------------


# Define the tidyverse pipe (%>%) so it can be used without attaching packages
`%>%` <- magrittr::`%>%`


# DEFINE FUNCTION TO RUN APP ---------------------------------------------------


runPPIApp <- function() {
    # Put together the UI for the app
    ui <- shiny::navbarPage(
        "PPI Visualization",

        # Create a tab to select and filter the dataset
        shiny::tabPanel(
            "Select and Filter a Dataset",

            selectDatasetTabUI("selectDatasetTab")
        ),

        # Create a tab to plot the interactions as heatmaps
        shiny::tabPanel(
            "Create Heatmaps",

            shiny::tabsetPanel(
                # Create a tab to select the variables to plot
                shiny::tabPanel(
                    "Select Variables",

                    selectVariablesTabUI("heatmapVariablesTab")
                ),

                # Create a tab to make and display a heatmap
                shiny::tabPanel(
                    "Create Heatmap",

                    heatmapTabUI("heatmapTab")
                )
            )
        ),

        # Create a tab to plot the interactions as barplots
        shiny::tabPanel(
            "Create Barplots",

            shiny::tabsetPanel(
                # Create a tab to select the variables to plot
                shiny::tabPanel(
                    "Select Variables",

                    selectVariablesTabUI("barplotVariablesTab")
                ),

                # Create a tab to make and display a barplot
                shiny::tabPanel(
                    "Create Barplot",

                    barplotTabUI("barplotTab")
                )
            )
        )

        # # Create a tab to plot the interactions as a network plot
        # shiny::tabPanel(
        #     "Create Network Plots",
        #
        #     networkTabUI("networkTab")
        # )
    )

    # Define the server logic
    server <- function(input, output, session) {
        # Get the filtered dataset
        filteredDataset <-
            selectDatasetTabServer("selectDatasetTab")

        # Select the variables for the heatmap and reformat the data accordingly
        heatmapInteractionCounts <-
            selectVariablesTabServer("heatmapVariablesTab", filteredDataset)

        # Make and display a heatmap
        heatmapTabServer("heatmapTab", heatmapInteractionCounts)

        # Select the variables for the barplot and reformat the data accordingly
        barplotInteractionCounts <-
            selectVariablesTabServer("barplotVariablesTab", filteredDataset)

        # Make and display a barplot
        barplotTabServer("barplotTab", barplotInteractionCounts)

        # # Make and display a network plot
        # networkTabServer("networkTab", filteredDataset)
    }

    # Run the app
    shiny::shinyApp(ui, server)
}

