## Script for running interactive PPI visualization app


## SET UP ----------------------------------------------------------------------


# Load necessary libraries
library("shiny")
library("shinyjs")
library("colourpicker")
library("DT")

# Do not convert columns of strings to factors when reading tables
options(stringsAsFactors = FALSE)

# Source files with function definitions
source("./PPI_heatmap.R", local = TRUE)
source("./PPI_network.R", local = TRUE)

# Source files with module definitions
source("./select_modules.R", local = TRUE)
source("./heatmap_modules.R", local = TRUE)
source("./network_modules.R", local = TRUE)
source("./reused_modules.R", local = TRUE)


## LOAD DATA -------------------------------------------------------------------


# Load lists of COF/TF PPIs (expects tab separated values and headers)
human_apid <-
    read.table("./data/human_APID_COF_TF.tsv", sep = "\t", header = TRUE)
mouse_apid <-
    read.table("./data/mouse_APID_COF_TF.tsv", sep = "\t", header = TRUE)
human_huri <-
    read.table("./data/human_HuRI_COF_TF.tsv", sep = "\t", header = TRUE)


# DEFINE UI --------------------------------------------------------------------


ui <- navbarPage(
    "PPI Visualization",
    
    # Make a tab to select and filter the dataset
    tabPanel(
        "Select and Filter Data",
        
        fluidPage(
            # Allow enabling/disabling of widgets
            useShinyjs(),
            
            sidebarLayout(
                # Make a sidebar to select dataset and add filters
                sidebarPanel(
                    # Select a dataset
                    select_data_UI("select_data"),
                    
                    # Add filters
                    filter_data_UI("filter_data")
                ),
                
                # Make a main panel to display the selected, filtered dataset
                mainPanel(
                    display_data_UI("display_data")
                )
            )
        )
    ),
    
    # Make a tab to generate heatmaps
    tabPanel(
        "Generate Heatmap",
        
        fluidPage(
            sidebarLayout(
                # Make a sidebar to generate a heatmap with specific settings
                sidebarPanel(
                    generate_heatmap_UI("generate_heatmap")
                ),
                
                # Make a main panel to display the heatmap
                mainPanel(
                    # Plot the heatmap
                    display_plot_UI("display_heatmap"),
                    
                    # Set up and download the heatmap as a pdf
                    download_plot_UI("download_heatmap")
                )
            )
        )
    ),
    
    # Make a tab to generate network graphs
    tabPanel(
        "Generate Network",
        
        fluidPage(
            sidebarLayout(
                # Make a sidebar to generate a network with specific settings
                sidebarPanel(
                    generate_network_UI("generate_network")
                ),
                
                # Make a main panel to display the heatmap
                mainPanel(
                    # Plot the heatmap
                    display_plot_UI("display_network"),
                    
                    # Set up and download the heatmap as a pdf
                    download_plot_UI("download_network")
                )
            )
        )
    )
)


## DEFINE SERVER ---------------------------------------------------------------


server <- function(input, output, session) {
    # Determine which dataset is selected
    dat <- select_data_server("select_data")
    
    # Get a list of all the filters to apply to the datset
    filters <- filter_data_server("filter_data", reactive(dat()))
    
    # Filter the dataset
    filtered_dat <- reactive({
        return(filter_rows(dat(), filters()))
    })
    
    # Display the filtered dataset
    display_data_server("display_data", reactive(filtered_dat()))
    
    # Generate the heatmap
    heatmap_plot <-
        generate_heatmap_server("generate_heatmap",
                                reactive(dat()),
                                reactive(filters()))
    
    # Display the heatmap
    display_plot_server("display_heatmap", reactive(heatmap_plot()))
    
    # Download the heatmap
    download_heatmap_server("download_heatmap", reactive(heatmap_plot()))
    
    # Make an igraph object for plotting the network
    dat_igraph <- reactive({
        extract_igraph(filtered_dat())
    })
    
    # Generate the network plot
    network_plot <-
        generate_network_server("generate_network",
                                reactive(dat_igraph()))
    
    # Display the network plot
    display_plot_server("display_network", reactive(network_plot()))
    
    # Download the network plot
    download_network_server("download_network", reactive(network_plot()))
}


## RUN APP ---------------------------------------------------------------------


shinyApp(ui, server)

