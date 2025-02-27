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
      column(2, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value")))
    ),
    actionButton(inputId = ns("click"), label = "Search"), # apply when this button is clicked
    #uiOutput(outputId = ns("panel")),
    uiOutput(outputId = ns("alert")), #for displaying alert messages #shinyjs
    #uiOutput(outputId = ns("coordinates_panel")), #for coordinates column

    fluidRow(
      box(title = "SNP Table", height = "600", solidHeader = T,
           column(1, align = "right", offset = 9,
                  downloadButton(outputId = ns("Download"), label = "Download table", icon = shiny::icon("download"))),
           column(2, align = "left", offset = 10,
                  selectInput(inputId = ns("filetype"), label = "File Type:", choices = c("csv", "tsv", "xlsx", "xls"))),
           DTOutput(outputId = ns("table_output"))
         ),#for table output
      br(),
      box(title = "SNP Plot analysis", height = "650", solidHeader = T,
          column(1, align = "right", offset = 8,
                 downloadButton(outputId = ns("download_bar"), label = "Download image", icon = shiny::icon("download"))),
          column(2, align = "left", offset = 10,
                 selectInput(inputId = ns("imgtype"), label = "Type:", choices = c("png", "pdf", "jpeg"))),
          column(12,
              div(height=500, width = 500,
                    plotOutput(outputId = ns("plot"), click = "plot_click", hover= "plot_hover")))
          )#for plot output
      ),
    #textInput(inputId = ns("text"), label ="type"),
    uiOutput(outputId = ns("text_plot"))
    )

}

#' name_of_module1 Server Functions
#'
#' @noRd
snp_table_server <- function(id, snps_df, snps_plot, df_combined) {
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

    # get the sample name of snps table
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
    #table_download <- reactiveValues(download=NULL) # for displaying table download option
    null_plot <- reactiveValues(imgplot=NULL) #true for NULL and false for displaying the plot

    # checking validity of the coordinates--------------------------
    observe({
          # browser()

        if (!isTruthy(start_coordTF()) || !isTruthy(end_coordTF())){
           # checking for numeric
           track_error_df$status <- reactive(FALSE)
           error_msg$msg <- "Provide numeric value for coordinates"
           output$table_output <- renderDT((NULL))
           output$plot <- renderPlot(NULL)
         }
         else {
           # browser()
           # only numeric values were provided
           start_coord$coord <- reactive(as.numeric(input$start_coord))
           end_coord$coord <- reactive(as.numeric(input$end_coord))

           if (!isTruthy(start_coord$coord() < end_coord$coord())) {
             track_error_df$status <- reactive(FALSE)
             error_msg$msg <- "End coordinate should be greater than start coordinate"
             output$table_output <- renderDT((NULL))
             output$plot <- renderPlot((NULL))
           }  else {
             output$alert <- renderText(NULL)
             track_error_df$status <- TRUE
             #null_plot$imgplot <- FALSE
             }
          if (is.null(start_coord$coord()) & is.null(end_coord$coord())) {
            null_table$show <- TRUE
            null_plot$imgplot <- TRUE
            output$table_output <- renderDT((NULL))
            output$plot <- renderPlot((NULL))

          } else {
           null_table$show <- FALSE
           null_plot$imgplot <- FALSE
         }
         }

      })


    # feedback ----------------------------------------------------------------

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
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    })





   # processing the final table--------------------------
    final_table <- eventReactive(input$click,{
      req(sample_name(), chr_sample(), start_coord$coord,
          end_coord$coord(), track_error_df$status)
     # plot_rct <- reactive(colnames(snps_plot))
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



     }  else if(!isTRUE(track_error_df$status) || !isTruthy(start_coord$coord()) || !isTruthy(end_coord$coord())){
        final_df <- as.data.frame(NULL)
      }
    })

    # display table--------------------------------
    observe({
      req(is.data.frame(final_table()))

      if(nrow(final_table()) > 1){
        output$table_output <- renderDT({
          datatable(
            cbind(final_table()),
            options = list(
              scrollX = TRUE,
              scrollY = "250px"
            ))


        })
      }
})



    # process the graph
      final_plot <- eventReactive(input$click, {
        req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample())
        # browser()


        # check for error and then proceed
        if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())) {

          # if no error, extract the data for plotting
          df_plot <- reactive({
            if(nrow(final_table()) > 1){
              df <- snps_plot %>% filter(chr == chr_sample() & (pos >= start_coord$coord() & pos <= end_coord$coord()))
            }else{
              df <- NULL
            }
            return(df)
          })

          if(nrow(df_plot()) > 1){
            data_plot <-  ggplot(data = df_plot(), aes(x = int_region ))+
              geom_bar(stat = "count")+
              theme_classic() +
              theme(
                axis.text.x = element_text(size = 10, face = "bold", angle = 90, hjust = 1),
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.title = element_text(size=10, face="bold"),
                axis.ticks = element_line(linewidth = 2)
              ) +
              labs(title="Analysis of snp data",
                   x = "Position",
                   y = "count")
               return(data_plot)
          }else{
            data_plot <- NULL
          }

        }
         else {
           data_plot <- NULL
         }
      })

        #display the graph
        observe({
          req(!is.null(final_plot()))
          # browser()
          output$plot <- renderPlot(final_plot())

            })

        observe({
          req(final_table(), input$plot_click)

           output$plot_text <- renderPrint({
             #browser()
             #if(is.null(input$plot_click$x)) return()
              y <- nearPoints(df = final_table(), coordinfo = input$plot_click, threshold = 10, maxpoints = 1, addRows = TRUE)
              if(nrow(y) != 0)
                return(y)

          })
        })

       # observe({
       #     req(!is.null(final_plot()))
       #   output$text_plot <- renderUI({
       #     click <- input$plot_click
       #     y <- nearPoints(final_table(), input$plot_click)
       #     req(nrow(y) != 0)
       #     DT::datatable(final_table(), colnames = (y), options = list(dom = '', searching = F, bSort = FALSE))
       #
       #   } )
       # })


        # Download action---------------------------------
        # Action for table
        observe({
          # browser()
          req(is.data.frame(final_table()))
          output$Download <- downloadHandler(

            filename = function() {

              paste(switch(input$filetype,
                           "csv" = "wheatdb_snps.csv",
                           "tsv" = "wheatdb_snps.tsv",
                           "xlsx" = "wheatdb_snps.xlsx",
                           "xls" = "wheatdb_snps.xls",
              ))
            },

            content = function(file) {

              if(input$filetype == "csv") {
                write.csv(final_table(), file, sep = ",")
              }
              else if(input$filetype == "tsv") {
                write.table(final_table(), file, sep = "\t")
              }
              else if(input$filetype == "xlsx" || input$filetype == "xls") {
                write.xlsx(final_table(), file)
              }
            }
          )

        })

        # action for graph
        observe({
          req(!is.null(final_plot()))
          imgtype <- reactive(NULL)
          # browser()
          output$download_bar <- downloadHandler(

            filename = function() {

              switch(input$imgtype,
                           "png" = paste0("snps_",format(Sys.time(), "%d_%X"),'.png'),
                           "pdf" = paste0("snps_",format(Sys.time(), "%d_%X"),'.pdf'),
                           "jpeg" = paste0("snps_",format(Sys.time(), "%d_%X"),'.jpeg'),

              )
            },

            content = function(file) {

              if(input$imgtype == "png") {
                png(file,  width = 12, height = 8, units = "in", res = 400)
                print(final_plot())
                dev.off()
                contentType = 'image/png'

              }
              else if(input$imgtype == "pdf") {
                pdf(file, width = 12, height = 8, onefile = T)
                print(final_plot())
                dev.off()
                contentType = 'image/pdf'

              }
              else if(input$imgtype == "jpeg") {
                jpeg(file,  width = 12, height = 8, units = "in", res = 400)
                print(final_plot())
                dev.off()
                contentType = 'image/jpeg'

              }
            }

          )




  } # end of inner module server
)
        } # end of module function


)}















## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
