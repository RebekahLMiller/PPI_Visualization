## File containing the module UI and server for making a heatmap


# DEFINE UI --------------------------------------------------------------------


makeHeatmapUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    shiny::tagList(
        # Create a text output box to specify which variables are being plotted
        shiny::textOutput(
            ns("plottedVariables")
        ),

        # Add some space between the text output and the title input
        shiny::br(),

        # Create a text input box to add a title
        shiny::textInput(
            ns("plotTitle"),
            label = "Add a title"
        ),

        # Create a text input box to add an x-axis label
        shiny::textInput(
            ns("xlab"),
            label = "Add a label to the x-axis"
        ),

        # Create a text input box to add a y-axis label
        shiny::textInput(
            ns("ylab"),
            label = "Add a label to the y-axis"
        ),

        # Create a color picker to choose the low end of the color gradient
        colourpicker::colourInput(
            ns("lowColor"),
            label = "Pick a color for the low end of the gradient",
            value = "lightgray"
        ),

        # Create a color picker to choose the high end of the color gradient
        colourpicker::colourInput(
            ns("highColor"),
            label = "Pick a color for the high end of the gradient",
            value = "midnightblue"
        ),

        # Create a checkbox to choose whether or not to display the legend
        shiny::checkboxInput(
            ns("showLegend"),
            label = "Should the legend be plotted?",
            value = TRUE
        ),

        # Show this panel only if the showLegend checkbox is checked
        shiny::conditionalPanel(
            condition = "input.showLegend == 1",

            # Create a text input box to a label to the legend
            shiny::textInput(
                ns("legendTitle"),
                label = "Add a label to the legend",
                value = "Number of Interactions"
            ),

            # Set the namespace
            ns = shiny::NS(id)
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


makeHeatmapServer <- function(id, interactionCounts) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure interactionCounts is a reactive
        stopifnot(shiny::is.reactive(interactionCounts))

        # Figure out the names of the variables to plot
        plottedVariables <-
            shiny::eventReactive(interactionCounts(), {
                # The row variable is the first column of the counts table
                rowVariable <-
                    colnames(interactionCounts())[1]

                # The column variable is the second to last column
                columnVariable <-
                    colnames(interactionCounts())[ncol(interactionCounts()) - 1]

                # Update the text saying which variables are being plotted
                output$plottedVariables <-
                    shiny::renderText(
                        paste(
                            "Plotting",
                            rowVariable,
                            "vs.",
                            columnVariable
                        )
                    )

                # Return a vector of the names of the variables to plot
                return(c(rowVariable, columnVariable))
            })

        # Make the heatmap
        heatmapPlot <-
            shiny::reactive({
                makeHeatmap(interactionCounts(), plottedVariables(), input)
            })

        # Return the heatmap
        return(heatmapPlot)
    })
}


# DEFINE HELPER FUNCTIONS ------------------------------------------------------


# Helper function to make a heatmap with ggplot2
# Inputs:
#   interactionCounts: the table of interaction counts to plot
#   plottedVariables: a vector containing the names of the variables to plot
#   input: the user inputs
makeHeatmap <- function(interactionCounts, plottedVariables, input) {
    heatmapPlot <-
        # Select the variables to plot as rows and columns
        ggplot2::ggplot(
            interactionCounts,
            ggplot2::aes(
                x = !!as.symbol(plottedVariables[2]),
                y = !!as.symbol(plottedVariables[1]),
                fill = number_unique_interaction_IDs
            )
        ) +

        # Plot heatmap tiles with a white border
        ggplot2::geom_tile(colour = "white") +

        # Set the overall theme to black and white
        ggplot2::theme_bw() +

        # Remove the axis ticks and gridlines, and adjust the x axis labels
        ggplot2::theme(
            axis.ticks = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 270, hjust = 0),
            panel.grid = ggplot2::element_blank()
        ) +

        # Add the axis labels, the legend label, and the title
        ggplot2::labs(
            x = input$xlab,
            y = input$ylab,
            fill = input$legendTitle,
            title = input$plotTitle
        ) +

        # Set the color gradient for the heatmap tiles
        ggplot2::scale_fill_gradient(
            low = input$lowColor,
            high = input$highColor
        )

    # Remove the legend if necessary
    if (!input$showLegend) {
        heatmapPlot <-
            heatmapPlot + ggplot2::guides(fill = FALSE)
    }

    # Return the final heatmap
    return(heatmapPlot)
}

