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
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    fluidRow(
      column(3,selectInput(inputId = ns("sample_name"), label = "Choose query type", choices = c("None"), selected = "None")),
      column(2, selectInput(inputId = ns("chr"), label = "Chromosome", choices = c("Chr1A", "Chr1B", "Chr1D",
                                                                                   "Chr2A", "Chr2B", "Chr2D",
                                                                                   "Chr3A", "Chr3B", "Chr3D",
                                                                                   "Chr4A", "Chr4B", "Chr4D",
                                                                                   "Chr5A", "Chr5B", "Chr5D",
                                                                                   "Chr6A", "Chr6B", "Chr6D",
                                                                                   "Chr7A", "Chr7B", "Chr7D"), selected = "NULL")),
      column(2, textInput(inputId = ns("start_coord"), label = "Coordinate Start", placeholder = c("Start value"))),
      column(2, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value"))),
      column(2, downloadLink(outputId = ns("Download"), label = "Download table", icon = shiny::icon("download"))),
      ),

    actionButton(inputId = ns("click"), label = "Search"), # apply when this button is clicked
    uiOutput(outputId = ns("panel")),
    #numericInput(inputId = ns("InputWarning"), "Warn if error happens", value=""),
    uiOutput(outputId = ns("alert")), #for displaying alert messages #shinyjs
    #helpText(outputId = ns("message"), style = "color:red"),
    uiOutput(outputId = ns("coordinates_panel")), #for coordinates column
    DTOutput(outputId = ns("table_output")), #for table output
    #downloadLink(outputId = ns("Download"))
)
}

#' name_of_module1 Server Functions
#'
#' @noRd
snp_table_server <- function(id, snps_df) {
  moduleServer(id, function(input, output, session){

    ns <- session$ns
    # browser()
    # check the snps_df and update the sample name menu
    sample_list <- reactive(colnames(snps_df))


    observe({
      req(is.data.frame(snps_df), sample_list())
      # browser()

      llst <- sample_list()[10:length(sample_list())]
      updateSelectInput(inputId = "sample_name", label = "Sample",
                        choices = c("All",llst))

    })

     # get the input  info  for coordinate and  return TRUE for numeric,  else FALSE for  non-numeric
    start_coordTF <- reactive({
      req(input$start_coord)
      yesNo <- sapply(input$start_coord, function(x) stringr::str_detect(x, "^[0-9]+$"))
      return(yesNo)
    })

    end_coordTF <- reactive({
      req(input$end_coord)
      yesNo <- sapply(input$end_coord, function(x) stringr::str_detect(x, "^[0-9]+$"))
      return(yesNo)
    })

    # to show the gene samples in table
    sample_name <- reactive(req(input$sample_name))
    chr_sample <- reactive(req(input$chr))

    # initialize start and end coordinate values
    start_coord <- reactiveValues(coord=NULL)
    end_coord <- reactiveValues(coord=NULL)
    # track the error: False for error msg and TRUE for no error
    track_error_df <- reactiveValues(status=NULL) # to get the error message in red and bold
    # error msg for coordinates
    error_msg <- reactiveValues(msg=NULL)
    #not to show the table when coordinates are empty
    null_table <- reactiveValues(show=NULL)

    # checking validity of the coordinates
    observe({

       # tryCatch({  # rework
      # browser()
      # if (is.null(start_coord$coord) & is.null(end_coord$coord)) {
      #  # output$table_output <- renderDT(NULL)
      #   #null_table$show <- TRUE
      # }

        if (!isTruthy(start_coordTF()) || !isTruthy(end_coordTF())){
           # checking for numeric
           #showFeedbackWarning(inputId = InputWarning)



           #output$alert <- renderText("Provide numeric value for coordinates" )
           track_error_df$status <- reactive(FALSE)
           error_msg$msg <- "Provide numeric value for coordinates"
           output$table_output <- renderDT((NULL))
           #stop("Provide numeric value for coordinate")
         }
         else {
           # browser()
           # only numeric values were provided
          #hideFeedback(inputId="start_coord",  )
           #output$alert <- renderText(NULL)
           #shinyjs::alert("click")
          # hideFeedback("InputWarning")
           # convert to numeric: textinput is character
           start_coord$coord <- reactive(as.numeric(input$start_coord))
           end_coord$coord <- reactive(as.numeric(input$end_coord))

           if (!isTruthy(start_coord$coord() < end_coord$coord())) {


             track_error_df$status <- reactive(FALSE)
             error_msg$msg <- "End coordinate should be greater than start coordinate"
             #output$alert <- renderText("End value should be greater than start value")
             #shinyjs::alert("End value should be greater than start value")
             output$table_output <- renderDT((NULL))
           }  else {
             #shinyjs::enable("click")
             output$alert <- renderText(NULL)
             track_error_df$status <- TRUE

           }
          if (is.null(start_coord$coord()) & is.null(end_coord$coord())) {
            null_table$show <- TRUE
            output$table_output <- renderDT((NULL))

          } else {
           null_table$show <- FALSE
         }
         }

        #  }, error = function(e) {
        #  paste("An error occured", e, "\n")
        # }

      })


    observe({
      req(track_error_df$status, error_msg$msg)
      if(!isTRUE(track_error_df$status)){
        showFeedbackWarning(inputId="start_coord", text = error_msg$msg, color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))
         track_error_df$status <- FALSE
        # error_msg$msg <- TRUE
      } else{
        hideFeedback(inputId = "start_coord")

      }
      output$table_output <- renderDT((NULL))
      null_table$show <- TRUE
    })





   # generating btable--------------------------
    final_table <- eventReactive(input$click,{
      req(sample_name(), chr_sample(), start_coord$coord,
          end_coord$coord(), track_error_df$status)
      # check for error and extract the data
      if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())){

        # first arrange proper column name
        if(sample_name() == "All"){
          col_list <- colnames(snps_df)
          df_sample <- reactive(snps_df)
        }else{
          col_list <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", sample_name())
          df_sample <- reactive(snps_df[,col_list])
        }

        final_df <- df_sample()[df_sample()$POS >= start_coord$coord() & df_sample()$POS <= end_coord$coord() & df_sample()$`#CHROM` == chr_sample(), col_list]
      }else if(!isTRUE(track_error_df$status) || !isTruthy(start_coord$coord()) || !isTruthy(end_coord$coord())){
        final_df <- as.data.frame(NULL)
      }
    })

    # display table
    observe({
      req(is.data.frame(final_table()))

      if(nrow(final_table()) > 1){
        output$table_output <- renderDT(datatable(final_table()))
      }
    })
    # observeEvent(input$click,  {
    #     # check the sample name provided by the user
    #     req(sample_name(), chr_sample(), !is.null(start_coord$coord),
    #         !is.null(end_coord$coord), isTRUE(track_error_df$status))
    #     browser()
    #     if(sample_name() == "All"){
    #       col_list <- colnames(snps_df)
    #       df_sample <- reactive(snps_df)
    #     }else{
    #       col_list <- c("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", sample_name())
    #       df_sample <- reactive(snps_df[,col_list])
    #     }
    #
    #     # browser()
    #     # check the start and end coordinates: start must always be smaller than end
    #     tryCatch({
    #       if(start_coord$coord < end_coord$coord) {
    #
    #       final_df <- df_sample()[df_sample()$POS >= start_coord$coord & df_sample()$POS <= end_coord$coord & df_sample()$`#CHROM` == chr_sample(), col_list]
    #       # print(start_coord$coord)
    #       # print(end_coord$coord)
    #
    #        output$table_output <- renderDT(datatable(final_df))
    #       }else if (start_coord$coord > end_coord$coord){
    #
    #         stop("End value should be greater than start value")
    #       #output$alert <- renderText("End values should be greater than start value")
    #       }
    #     #show the table based on column
    #     }, error = function(e) {
    #       paste("An error occured", e, "\n")
    #     }
    #     )
    #
    #   })


 }#module server
)
  }#snp table server


## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
