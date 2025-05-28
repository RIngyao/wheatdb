#' name_of_module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
blast_ui <- function(id) {
  ns <- NS(id)
  tagList(

   div(
     style= "margin-top:2px;
      background-image: linear-gradient(to bottom right, white, #c1d4d1);
      padding:5px;
      text-align:left;
      align:left;
      font-weight:bold;
      ",

     h2("Enter FASTA", style = "margin-top:1px; text-align:left; font-weight:bold; color:#025b05"),

     fluidRow(
       column(8,
              column(8, textAreaInput(inputId = ns("query_seq"), label = "Enter accession number(s) or FASTA sequence(s)")),
              column(8, fileInput(inputId = ns("file_blast"), label = "Or, upload file", placeholder = c("No file chosen")))
       ),

     )
   ),

    div(
      style= "margin-top:2px;
      background-image: linear-gradient(to bottom right, white, #c1d4d1);
      padding:5px;
      text-align:left;
      align:left;
      font-weight:bold;
      ",

      h2("General Parameters", style = "margin-top:1px; text-align:left; font-weight:bold; color:#025b05"),
      fluidRow(
        column(6, selectInput(inputId = ns("max_target"), label = "Max target sequences", choices = c("10", "50", "100", "250", "500", "1000", "5000"))),
        column(6,
               h4("Short Queries"),
               checkboxInput(inputId = ns("query"),
                                label = tagList(
                                  "Automatically adjust parameters for short input sequences",
                                  tags$span(
                                    icon("question-circle"),
                                         title = "Enable this to system optimise the sttings")
                                  )
                                ),
                                 value = TRUE
                                ),
        column(6, textInput(inputId = ns("exp_thres"), label = "Expect threshold")),
        column(6, selectInput(inputId = ns("size"), label = "Word size", choices = c("16", "20", "24", "28", "32", "48", "64"))),
        column(6, textInput(inputId = ns("query_match"), label = "Max matches in a query range"))
    )
    ),

    div(
      style= "margin-top:2px;
      background-image: linear-gradient(to bottom right, white, #c1d4d1);
      padding:5px;
      text-align:left;
      align:left;
      font-weight:bold;
      ",

       h2("Scoring Parameters", style = "margin-top:1px; text-align:left; font-weight:bold; color:#025b05"),
       fluidRow(
         column(6, selectInput(inputId = ns("max_target"), label = "Match/Mismatch Scores", choices = c("10", "50", "100", "250", "500", "1000", "5000"))),
         column(6, selectInput(inputId = ns("gap"), label = "Gap Costs", choices = c("Linear", "Existence:5 Extension:2", "Existence:2 Extension:2",
                                                                                     "Existence:1 Extension:2", "Existence:0 Extension:2"), selected = "Linear"))

     )
     ),

       actionButton(inputId = ns("click"), label = "BLAST", width = "10%", class = "btn-info btn-sm", style = "font-weight:bold; align: center;")
     )

}

#' name_of_module2 Server Functions
#'
#' @noRd
blast_server <- function(id, blast_df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



  })
}

## To be copied in the UI
# mod_name_of_module2_ui("name_of_module2_1")

## To be copied in the server
# mod_name_of_module2_server("name_of_module2_1")
