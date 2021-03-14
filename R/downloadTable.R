## File containing the module UI and server for downloading a table


# DEFINE UI --------------------------------------------------------------------


downloadTableUI <- function(id) {
    # Set the namespace
    ns <- shiny::NS(id)

    shiny::tagList(
        # Create a button to download the plot as a pdf
        shiny::downloadButton(
            ns("download")
        )
    )
}


# DEFINE SERVER ----------------------------------------------------------------


downloadTableServer <- function(id, downloadTable, fileName) {
    shiny::moduleServer(id, function(input, output, session) {
        # Make sure downloadTable is a reactive and fileName is not
        stopifnot(shiny::is.reactive(downloadTable))
        stopifnot(!shiny::is.reactive(fileName))

        # Download the table as a tsv file when the download button is clicked
        output$download <-
            shiny::downloadHandler(
                fileName,
                function(file) {
                    # Save the table as a tsv file
                    write.table(
                        downloadTable(),
                        file,
                        quote = FALSE,
                        row.names = FALSE,
                        sep = "\t"
                    )
                }
            )
    })
}

