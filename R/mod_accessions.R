#' accessions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_accessions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "List of accessions/varieties available in the database",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        div(style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
            downloadButton(ns("download_accession"), "Download Table")
        ),
        DTOutput(ns("accession_table"))
      )
    )
  )
}

#' accessions Server Functions
#'
#' @noRd
mod_accessions_server <- function(id, accession_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render datatable
    output$accession_table <- renderDT({
      req(accession_data)

      datatable(
        accession_data,
        rownames = TRUE,
        colnames = c("Name", "Group"),
        filter = "top",
        extensions = 'Buttons',
        options = list(
          pageLength = 200,
          autoWidth = FALSE,
          scrollX = TRUE
        )
      )
    })

    # Download handler for CSV
    output$download_accession <- downloadHandler(
      filename = function() {
        paste0("accession_table_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
      },
      content = function(file) {
        write.csv(accession_data, file, row.names = FALSE)
      }
    )
  })
}

## To be copied in the UI
# mod_accessions_ui("accessions_1")

## To be copied in the server
# mod_accessions_server("accessions_1")
