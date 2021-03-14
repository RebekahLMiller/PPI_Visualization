## File containing the module UI and server for making a barplot


# DEFINE UI --------------------------------------------------------------------


makeBarplotUI <- function(id) {
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

        # Create a checkbox to choose whether or not to facet_wrap the plot
        shiny::checkboxInput(
            ns("facetWrap"),
            label = "Should the plot be grouped?",
            value = FALSE
        ),

        # Create a checkbox to choose whether or not to color the plot
        shiny::checkboxInput(
            ns("colorPlot"),
            label = "Should the plot be colored by the grouping variable?",
            value = TRUE
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
                label = "Add a label to the legend"
            ),

            # Set the namespace
            ns = shiny::NS(id)
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


makeBarplotServer <- function(id, interactionCounts) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure interactionCounts is a reactive
        stopifnot(shiny::is.reactive(interactionCounts))

        # Figure out the names of the variables to plot
        plottedVariables <-
            shiny::eventReactive(interactionCounts(), {
                # The main plot variable is the first column of the counts table
                plotVariable <-
                    colnames(interactionCounts())[1]

                # The grouping variable is the second to last column
                groupingVariable <-
                    colnames(interactionCounts())[ncol(interactionCounts()) - 1]

                # Do not split up the plot right after the dataset changes
                shiny::updateCheckboxInput(
                    session,
                    "facetWrap",
                    value = FALSE
                )

                # Update the text saying which variables are being plotted
                output$plottedVariables <-
                    shiny::renderText(
                        paste(
                            "Plotting interaction counts per",
                            plotVariable,
                            "optionally grouped by",
                            groupingVariable
                        )
                    )

                # Return a vector of the names of the variables to plot
                return(c(plotVariable, groupingVariable))
            })

        # Make the barplot
        barplotPlot <- shiny::reactive({
            return(makeBarplot(
                interactionCounts(), plottedVariables(), input))
        })

        # Return the barplot
        return(barplotPlot)
    })
}


# DEFINE HELPER FUNCTIONS ------------------------------------------------------


# Helper function to make a barplot with ggplot2
# Inputs:
#   interactionCounts: the table of interaction counts to plot
#   plottedVariables: a vector containing the names of the variables to plot
#   input: the user inputs
makeBarplot <- function(interactionCounts, plottedVariables, input) {
    barplotPlot <-
        # Select the variables to plot as rows and columns
        ggplot2::ggplot(
            interactionCounts,
            ggplot2::aes(
                # Reorder the x-axis by the number of interactions
                x = reorder(
                    !!as.symbol(plottedVariables[1]),
                    -number_unique_interaction_IDs,
                    sum
                ),
                y = number_unique_interaction_IDs,
                fill = !!as.symbol(plottedVariables[2])
            )
        ) +

        # Plot barplot
        ggplot2::geom_bar(stat = "identity") +

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
        )

    # Split the plot up by the grouping variable if necessary
    if (input$facetWrap) {
        barplotPlot <-
            barplotPlot +
            ggplot2::facet_wrap(as.formula(paste("~", plottedVariables[2])))
    }

    # Remove the grouping variable coloring if necessary
    if (!input$colorPlot) {
        barplotPlot <-
            barplotPlot +
            ggplot2::scale_fill_manual(
                values = rep(
                    "midnightblue",
                    length(unique(interactionCounts[[plottedVariables[2], ]]))
                    )
                )
    }

    # Remove the legend if necessary
    if (!input$showLegend) {
        barplotPlot <-
            barplotPlot +
            ggplot2::guides(fill = FALSE)
    }

    # Return the final heatmap
    return(barplotPlot)
}

