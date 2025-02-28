#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#
#' @importFrom shiny NS tagList
snp_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    selectInput(inputId = ns("sample_name"), label = "SAMPLE NAME", choices = c("None"), selected = "None"),
    selectInput(inputId = ns("query_menu"), label = "Dropdown Menu", choices = c("None", "geneID", "type", "impact", "coordinates")),
    uiOutput(outputId = ns("filterInput")), #dynamic output for text display when user's choice is selected
    actionButton(inputId = ns("click"), label = "Submit"), # apply when this button is clicked

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

      llst <- sample_list()[8:length(sample_list())]
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
    gene_sample <- reactive(req(input$gene_name))
    sample_name <- reactive(req(input$sample_name))
    type_sample <- reactive(req(input$type_name))
    impact_sample <- reactive(req(input$impact_name))
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
      req(track_error_df$status, error_msg$msg, gene_error$message)
      if(!isTRUE(track_error_df$status)){
        showFeedbackWarning(inputId="start_coord", text = error_msg$msg, color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))
        showFeedbackWarning(inputId = "gene_name", text = gene_error$message, color = #ff0000",
                              icon = shiny::icon("warning-sign", lib = "glyphicon"))
         track_error_df$status <- FALSE
        # error_msg$msg <- TRUE
      } else{
        hideFeedback(inputId = "start_coord")
        hideFeedback(inputId = "gene_name")
      }
      output$table_output <- renderDT((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    })


  #checking the query input menu--------------------------------------------

    output$filterInput <- renderUI({
    if(input$query_menu == "geneID") {
      textInput(inputId = ns("gene_name"), label = "GENE ID", placeholder = c("Should be comma or space separated only"))
    }
    else if (input$query_menu == "type")  {
      tagList(
        fluidRow(
          column(4, selectInput(inputId = ns("type_name"), label = "TYPE", choices = c("intergenic_region", "upstream_gene_variant",
                                                                             "intron_variant", "conservative_inframe_deletion",
                                                                             "3_prime_UTR_variant", "5_prime_UTR_variant",
                                                                             "downstream_gene_variant", "splice_region_variant&intron_variant",
                                                                             "frameshift_variant", "conservative_inframe_insertion",
                                                                             "disruptive_inframe_insertion", "frameshift_variant&stop_gained",
                                                                             "frameshift_variant&splice_region_variant", "stop_gained&conservative_inframe_insertion",
                                                                             "disruptive_inframe_deletion", "non_coding_transcript_exon_variant",
                                                                             "missense_variant", "frameshift_variant&start_lost",
                                                                             "conservative_inframe_insertion&splice_region_variant", "splice_acceptor_variant&splice_region_variant&intron_variant",
                                                                             "splice_acceptor_variant&intron_variant", "splice_region_variant",
                                                                             "splice_donor_variant&intron_variant", "start_lost&conservative_inframe_insertion"))),
          column(4, selectInput(inputId = ns("chr"), label = "Chromosome", choices = c("Chr1A", "Chr1B", "Chr1D",
                                                                             "Chr2A", "Chr2B", "Chr2D",
                                                                             "Chr3A", "Chr3B", "Chr3D",
                                                                             "Chr4A", "Chr4B", "Chr4D",
                                                                             "Chr5A", "Chr5B", "Chr5D",
                                                                             "Chr6A", "Chr6B", "Chr6D",
                                                                             "Chr7A", "Chr7B", "Chr7D"), selected = "NULL")),
          column(2, textInput(inputId = ns("start_coord"), label = "Coordinate Start", placeholder = c("Start value"))),
          column(2, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value")))
        )
      )


    } else if(input$query_menu == "impact") {
      tagList(
        fluidRow(
        column(4, selectInput(inputId = ns("impact_name"), label = "IMPACT", choices = c("MODIFIER", "MODERATE", "LOW", "HIGH"))),
        column(4, selectInput(inputId = ns("chr"), label = "Chromosome", choices = c("Chr1A", "Chr1B", "Chr1D",
                                                                                     "Chr2A", "Chr2B", "Chr2D",
                                                                                     "Chr3A", "Chr3B", "Chr3D",
                                                                                     "Chr4A", "Chr4B", "Chr4D",
                                                                                     "Chr5A", "Chr5B", "Chr5D",
                                                                                     "Chr6A", "Chr6B", "Chr6D",
                                                                                     "Chr7A", "Chr7B", "Chr7D"), selected = "NULL")),
        column(2, textInput(inputId = ns("start_coord"), label = "Coordinate Start", placeholder = c("Start value"))),
        column(2, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value")))
        )
      )


    } else if(input$query_menu == "coordinates") {
      tagList(
      fluidRow(
        column(4, selectInput(inputId = ns("chr"), label = "Chromosome", choices = c("Chr1A", "Chr1B", "Chr1D",
                                                                                     "Chr2A", "Chr2B", "Chr2D",
                                                                                     "Chr3A", "Chr3B", "Chr3D",
                                                                                     "Chr4A", "Chr4B", "Chr4D",
                                                                                     "Chr5A", "Chr5B", "Chr5D",
                                                                                     "Chr6A", "Chr6B", "Chr6D",
                                                                                     "Chr7A", "Chr7B", "Chr7D"), selected = "NULL")),
        column(2, textInput(inputId = ns("start_coord"), label = "Coordinate Start", placeholder = c("Start value"))),
        column(2, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value")))
      ))
    }
      else {
        NULL
      }

    })




   # processing the final table--------------------------

     final_table <- eventReactive(input$click,{
      req(sample_name()) #, type_sample(), impact_sample(), gene_sample(),
          #start_coord$coord(), end_coord$coord(), track_error_df$status)
       # browser()

       # first arrange proper column name
       if(sample_name() == "All") {
         col_list <- colnames(snps_df)
         df_sample <- reactive(snps_df)
       }
       else{
         col_list <- c("#CHROM", "POS", "REF", "ALT", "TYPE", "IMPACT", "GENE_ID", sample_name())
         df_sample <- reactive(snps_df[,col_list])
       }

       query <- reactive(input$query_menu)
      # check for error and extract the data
       if(query() == "geneID") {

         # query with only gene ID
         req(gene_sample())
         gene <- unlist(strsplit(gene_sample(), "[, ]+"))  #splitting the gene on the basis of comma or space
         print(gene)
         # validate
         if(!is.integer(grep("TraeCS", gene))){
           cap_err <- "Invalid gene ID. Eg. TraesCS1A03G0011000"
         }else if (nchar(gene) != 19){
           cap_err <- "Invalid gene ID. It must be 19 in length"
         }else{
           df_table <- df_sample()[df_sample()$GENE_ID %in% gene, col_list]
         }
       }
       else{

         # query with coordinates
         if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())){

           if(query() == "coordinates") {

             req(start_coord$coord(), end_coord$coord(), chr_sample())
             df_table  <- df_sample()[df_sample()$POS >= start_coord$coord() & df_sample()$POS <= end_coord$coord() & df_sample()$`#CHROM` == chr_sample(), col_list]
           }
           else if(query() == "type") {
             req(type_sample(), start_coord$coord(), end_coord$coord(), chr_sample())
             df_table <- df_sample()[df_sample()$POS >= start_coord$coord() & df_sample()$POS <= end_coord$coord() & df_sample()$TYPE == type_sample() & df_sample()$`#CHROM` == chr_sample(), col_list]
           }
           else if(query() == "impact") {
             req(chr_sample(), impact_sample(), start_coord$coord(), end_coord$coord(), track_error_df$status)
             df_table <- df_sample()[df_sample()$POS >= start_coord$coord() & df_sample()$POS <= end_coord$coord() & df_sample()$IMPACT == impact_sample() & df_sample()$`#CHROM` == chr_sample(), col_list]

           }

         }else {
           # Default will be null for table
           df_table <- as.data.frame(NULL)
         } # end of inner if clause

       }# end of if clause

       return(df_table)

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



    # # process the graph
    #   final_plot <- eventReactive(input$click, {
    #     req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample(), type_sample())
    #     # browser()
    #
    #
    #     # check for error and then proceed
    #     if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())) {
    #
    #       # if no error, extract the data for plotting
    #       df_plot <- reactive({
    #         if(nrow(final_table()) > 1){
    #           df <- snps_df %>% filter(`#CHROM` == chr_sample() & (POS >= start_coord$coord() & POS <= end_coord$coord()) & TYPE == type_sample())
    #         }else{
    #           df <- NULL
    #         }
    #         return(df)
    #       })
    #
    #       if(nrow(df_plot()) > 1){
    #         data_plot <-  ggplot(data = df_plot(), aes(x = TYPE))+
    #           geom_bar(stat = "count")+
    #           theme_classic() +
    #           theme(
    #             axis.text.x = element_text(size = 10, face = "bold", angle = 90, hjust = 1),
    #             axis.text.y = element_text(size = 10, face = "bold"),
    #             axis.title = element_text(size=10, face = "bold"),
    #             axis.ticks = element_line(linewidth = 2)
    #           ) +
    #           labs(title="Analysis of snp data",
    #                x = "Position",
    #                y = "count")
    #            return(data_plot)
    #       }else{
    #         data_plot <- NULL
    #       }
    #
    #     }
    #      else {
    #        data_plot <- NULL
    #      }
    #   })
    #
    #     #display the graph
    #     observe({
    #       req(!is.null(final_plot()))
    #       # browser()
    #       output$plot <- renderPlot(final_plot())
    #
    #         })





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

#         # action for graph
#         observe({
#           req(!is.null(final_plot()))
#           imgtype <- reactive(NULL)
#           # browser()
#           output$download_bar <- downloadHandler(
#
#             filename = function() {
#
#               switch(input$imgtype,
#                            "png" = paste0("snps_",format(Sys.time(), "%d_%X"),'.png'),
#                            "pdf" = paste0("snps_",format(Sys.time(), "%d_%X"),'.pdf'),
#                            "jpeg" = paste0("snps_",format(Sys.time(), "%d_%X"),'.jpeg'),
#
#               )
#             },
#
#             content = function(file) {
#
#               if(input$imgtype == "png") {
#                 png(file,  width = 12, height = 8, units = "in", res = 400)
#                 print(final_plot())
#                 dev.off()
#                 contentType = 'image/png'
#
#               }
#               else if(input$imgtype == "pdf") {
#                 pdf(file, width = 12, height = 8, onefile = T)
#                 print(final_plot())
#                 dev.off()
#                 contentType = 'image/pdf'
#
#               }
#               else if(input$imgtype == "jpeg") {
#                 jpeg(file,  width = 12, height = 8, units = "in", res = 400)
#                 print(final_plot())
#                 dev.off()
#                 contentType = 'image/jpeg'
#
#               }
#             }
#
#           )
#
#
#
#
#   } # end of inner module server
# )
        } # end of module function


)}










#& df_sample()$TYPE == type_sample() & df_sample()$IMPACT == impact_sample(), df_sample()$GENE_ID == gene_sample()


#"TraesCS1A03G0011000"
# col_list
## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
