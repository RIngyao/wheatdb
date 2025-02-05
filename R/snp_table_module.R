
library(DT)
#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
snp_table_ui <- function(id) {
  ns <- NS(id)
  tagList(

    selectInput(inputId = ns("query_menu"), label = "Choose query type", choices = c("None", "Gene", "Coordinates"), selected = "None"),
    #textInput(inputId = ns("alert_text"), label= "Enter a alert text"),
    uiOutput(outputId = ns("panel")),
    uiOutput(outputId = ns("alert")),
    uiOutput(outputId = ns("coordinates_panel")),
   #  textInput(inputId = ns("EnterValue"), label= "Enter a value"), # user's input
   # numericInput(inputId = ns("num"), label = "Enter a numeric value", value = NULL, min = 0, max = 100), # user's numeric input
   # selectInput(inputId = ns("variable"), label = "Choose columns", choices = c("Sepal width", "Sepal Length", "Petal Length", "Petal Width")), # user's choice of columns
   #actionButton(inputId = ns("click"), label = "Click to proceed"), # apply when this button is clicked
   #DTOutput(ns("table_output")), # table output for the input variables
   #textOutput(ns("result")), # output when button is clicked
   # verbatimTextOutput(ns("num_value")), # table output for the numeric input
   # verbatimTextOutput(ns("print")) # table output for the user's choice input
)
}

#' name_of_module1 Server Functions
#'
#' @noRd
snp_table_server <- function(id){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

   # browser()
    # get usr input for gene or coordinates
    gene_coord <- reactive(req(input$query_menu))
    #browser()


    observe({
      req(gene_coord())


      if (!is.numeric(gene_coord())){
        output$alert <- renderText({"Input is not numeric" })
      }
      else {
        output$alert <- renderText(NULL)
      }

      print(gene_coord())
      if(gene_coord() == "Gene")
        {
          output$panel <- renderUI(
            textInput(inputId = NS(id, "gene"), label = c("Gene ID"), placeholder = c("only one ID"))
            )
          }
      else if (gene_coord() == "Coordinates"){
            output$panel <- renderUI(
              tagList(textInput(inputId = NS(id, "coordinates"), label = "start", placeholder = c("start value")),
                      textInput(inputId = NS(id, "coordinates"), label = "end", placeholder = c("end value"))
                      )


            )}
      else{
              output$panel <- renderUI(NULL)
            }
    })
    # observeEvent(req(gene_coord()),{
    #   # browser()
    #   print(gene_coord())
    #   if(gene_coord() == "Gene"){
    #     output$panel <- renderUI(
    #       selectInput(inputId = NS(id, "try"), "Gene", choices = c("jac", "la"))
    #     )
    #   }else if (gene_coord() == "Coordinates"){
    #     output$panel <- renderUI(
    #       selectInput(inputId = NS(id, "try"), "Coordinates", choices = c("jsssac", "ssf"))
    #     )
    #   }
    #
    # })
    # observeEvent(input$click, {
    #   output$result <- renderText("Botton clicked")
    #   output$num_value <- renderText({ input$numeric })
    #   output$print <- renderText({ input$variable})
    #   #browser()
    #
    #   selected_data <- reactive({
    #
    #     #browser()
    #     value <- input$EnterValue
    #     number <- input$num
    #
    #     #browser()
    #     if(is.numeric(number)){
    #       #code for numeric
    #       df <- iris[iris$Sepal.Length <= number, ]
    #     }else if(is.character(value)){
    #       df <- iris[iris$Species == value, ]
    #     }
    #
    #
    #      else
    #      df <- iris[list(iris)]
    #
    #
    #
    #     return(df)
    #
    #
    #   })
    #
    #   # browser()
    #   output$table_output <- renderDT(
    #     datatable(selected_data(), options = list(pageLength = 10))
    #   )
    #
    # })



 }
)
} #snp_table_server

## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
