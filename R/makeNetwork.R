## File containing the module UI and server for making a network plot


# DEFINE UI --------------------------------------------------------------------


makeNetworkUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    shiny::tagList(
        # Create radio buttons to select a layout for the network
        shiny::radioButtons(
            ns("layout"),
            "Select a network layout",
            choices = c("sphere", "lgl", "circle", "grid", "kk", "fr"),
            selected = "sphere"
        ),

        # Create a dropdown menu of columns to use to label the nodes
        shiny::selectInput(
            ns("nodeLabel"),
            "Choose a column to use to label the nodes",
            choices = NA
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


makeNetworkServer <- function(id, filteredDataset) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure filteredDataset is a reactive
        stopifnot(shiny::is.reactive(filteredDataset))

        # Create an igraph object from the filteredDataset
        igraphObject <-
            shiny::reactive({
                makeIgraph(filteredDataset())
            })

        # Update the choices for the node labels whenever igraphObject changes
        shiny::observeEvent(igraphObject(), {
            # Update the node label dropdown menu
            shiny::updateSelectInput(
                session,
                "nodeLabel",
                choices = c(NA, igraph::vertex_attr_names(igraphObject()))
            )
        })

        # Make the network plot
        networkPlot <-
            shiny::reactive({
                plotNetwork(igraphObject(), input)
            })

        # Return the network plot
        return(networkPlot)
    })
}


# DEFINE HELPER FUNCTIONS ------------------------------------------------------


# Helper function to make an igraph object out of the filtered dataset
# Inputs:
#   filteredDataset: the filtered dataset to convert to an igraph object
makeIgraph <- function(filteredDataset) {
    # Get a data frame containing the edge attributes
    edgeList <-
        getEdges(filteredDataset)

    # Get a data frame containing the vertex attributes
    vertexList <-
        getVertices(filteredDataset)

    # Create the igraph object
    igraphObject <-
        igraph::graph_from_data_frame(edgeList, vertices = vertexList)

    # Return the igraph object
    return(igraphObject)
}

# Helper function to make a data frame containing the edge attributes
# Inputs:
#   filteredDataset: the filtered dataset to convert to an igraph object
getEdges <- function(filteredDataset) {
    # Make a data frame containing the edge attributes
    edgeList <-
        filteredDataset %>%

        # Remove all the information specific to the nodes (COFs/TFs)
        dplyr::select(
            -"Interaction_ID",
            -dplyr::contains("COF"),
            -dplyr::contains("TF"),
            "COF_name",
            "TF_name"
        ) %>%

        # Reorder the columns so the COF_name and TF_name columns are first
        dplyr::select(
            "COF_name",
            "TF_name",
            colnames(.)
        ) %>%

        # Remove duplicate rows
        dplyr::distinct()

    # Return the edge attributes data frame
    return(edgeList)
}

# Helper function to make a data frame containing the vertex attributes
# Inputs:
#   filteredDataset: the filtered dataset to convert to an igraph object
getVertices <- function(filteredDataset) {
    # Make a data frame containing the COF attributes
    cofList <-
        filteredDataset %>%

        # Remove all the information not specific to the COFs
        dplyr::select(
            name = "COF_name",
            synonyms = "COF_synonyms",
            dplyr::contains("COF")
        ) %>%

        # Remove duplicate rows
        dplyr::distinct()

    # Make a data frame containing the TF attributes
    tfList <-
        filteredDataset %>%

        # Remove all the information not specific to the TFs
        dplyr::select(
            name = "TF_name",
            synonyms = "TF_synonyms",
            dplyr::contains("TF")
        ) %>%

        # Remove duplicate rows
        dplyr::distinct()

    # Combine the COF and TF vertex attribute data frames
    vertexList <-
        plyr::rbind.fill(cofList, tfList)

    # Collapse rows so that each vertex has only one row
    vertexList <-
        vertexList %>%
        aggregate.data.frame(
            by = list(vertexList[, 1]),
            FUN = function(column) {
                # Return NA if all the matching rows have NA in this column
                if (all(is.na(column))) {
                    return(NA)
                }

                # Remove NA values and collapse the remaining unique values
                collapsed_values <-
                    column %>%
                    purrr::discard(is.na) %>%
                    unique() %>%
                    paste(collapse = ";")

                # Return string of collapsed values
                return(collapsed_values)
            }
        )

    # Return the vertex attributes data frame
    return(vertexList[-1])
}

# Helper function to plot the network using ggraph
# Inputs:
#   igraphObject: the igraph object representing the network to plot
#   input: the user inputs
plotNetwork <- function(igraphObject, input) {

    # Start the network plot and select the layout
    networkPlot <-
        ggraph::ggraph(igraphObject, layout = input$layout) +

        # Add the edges
        ggraph::geom_edge_link() +

        # Add the vertices
        ggraph::geom_node_point() +

        # Set the overall theme to void
        ggplot2::theme_void()

    # Add node labels if necessary
    if (input$nodeLabel != "NA") {
        networkPlot <-
            networkPlot +
            ggraph::geom_node_label(
                ggplot2::aes(
                    label = tidyr::replace_na(
                        igraph::vertex_attr(igraphObject, input$nodeLabel),
                        ""
                    )
                ),
            repel = TRUE)
    }

    # Return the final network plot
    return(networkPlot)
}

