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
  # important variables------------------------------
  # type for SNPs
  type <- c("intergenic_region", "upstream_gene_variant",
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
            "splice_donor_variant&intron_variant", "start_lost&conservative_inframe_insertion")
  chrom <- c("Chr1A", "Chr1B", "Chr1D",
             "Chr2A", "Chr2B", "Chr2D",
             "Chr3A", "Chr3B", "Chr3D",
             "Chr4A", "Chr4B", "Chr4D",
             "Chr5A", "Chr5B", "Chr5D",
             "Chr6A", "Chr6B", "Chr6D",
             "Chr7A", "Chr7B", "Chr7D")
  # start ui-------------------------
  tagList(
    shinyFeedback::useShinyFeedback(),
    # box for query parameters------------------------
    div(
      style= "margin-top:2px;
      background-image:radial-gradient(white,white, #dbf1f9);
      box-shadow:0px 2px 20px -15px inset;
      padding:5px;
      text-align:center;
      align:center;
      font-weight:bold;
      ",
      h2("Query Parameters", style = "margin-top:1px; text-align:center; font-weight:bold; color:#025b05"),
      div(
        style ="display:flex;
        justify-content:center;
        padding: 5px;",

        fluidRow(
          column(6, selectInput(inputId = ns("query_menu"), label = "Query type", choices = c("None", "geneID", "type", "impact", "coordinates"))),
          column(6,selectInput(inputId = ns("sample_name"), label = "Cultivar name", choices = c("None"), selected = "None"))
        )
      ),

      div(
        style = "display:flex; justify-content:center; padding: 5px",

        conditionalPanel(condition = sprintf("input['%s'] == 'geneID'", ns("query_menu")),

                         # change the button type: use raditobutton
                         fluidRow(
                           column(4, radioButtons(inputId = ns("gene_name"), label = "Choose", choices = c("Enter", "Upload"))),

                          conditionalPanel(condition = sprintf("input['%s'] == 'Enter'", ns("gene_name")),
                                           column(8,textInput(inputId = ns("enter_gene"), label = "Enter GENE ID", placeholder = c("Must be comma or space separated!")))
                                           ),

                          conditionalPanel(condition = sprintf("input['%s'] == 'Upload'", ns("gene_name")),
                                             column(8,fileInput(inputId = ns("upload"), label = "Upload GENE ID", placeholder = "No files selected" ))
                                             )

                         )

                         ), # end of conditionpanel geneID

        # ui for type-----------------------
        conditionalPanel(condition = sprintf("input['%s'] == 'type' ", ns("query_menu")),
                           tagList(
                             fluidRow(
                               column(4, selectInput(inputId = ns("type_name"), label = "TYPE", choices = type)),
                               column(4, selectInput(inputId = ns("chr"), label = "Chromosome", choices = chrom, selected = "Chr1A")),
                               column(2, textInput(inputId = ns("start_coord"), label = "Coordinate Start", placeholder = c("Start value"))),
                               column(2, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value")))
                             )
                           )

                         ), # end of type

        # ui for coorindates -----------------------
        conditionalPanel(condition = sprintf("input['%s'] == 'impact' ", ns("query_menu")),
                         tagList(
                           fluidRow(
                             column(4, selectInput(inputId = ns("impact_name"), label = "IMPACT", choices = c("MODIFIER", "MODERATE", "LOW", "HIGH"))),
                             column(4, selectInput(inputId = ns("chr"), label = "Chromosome", choices = chrom, selected = "Chr1A")),
                             column(2, textInput(inputId = ns("start_coord"), label = "Coordinate Start", placeholder = c("Start value"))),
                             column(2, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value")))
                           )
                         )

        ), # end of impact

        conditionalPanel(condition = sprintf("input['%s'] == 'coordinates' ", ns("query_menu")),
                         tagList(
                           fluidRow(
                             column(4, selectInput(inputId = ns("chr"), label = "Chromosome", choices = chrom, selected = "Chr1A")),
                             column(4, textInput(inputId = ns("start_coord"), label = "Coordinate Start", placeholder = c("Start value"))),
                             column(4, textInput(inputId = ns("end_coord"), label = "Coordinate End", placeholder = c("End value")))
                           )
                         )

        ) # end of coordinates

      ),

      # button for submitting the query
      conditionalPanel(condition = sprintf("input['%s'] != 'None'", ns("query_menu")),
                       actionButton(inputId = ns("click"), label = "SUBMIT",
                                    width = "70%",
                                    # class = "btn-primary btn-lg",
                                    class = "btn-info btn-sm",
                                    style = "font-weight:bold;")
                       )

    ),


    # display the download button only when the data is ready (table and figure)
     fluidRow(
      box( height = "600", solidHeader = T, #title = "SNP Table",

           column(1, align = "right", offset = 9,
                  uiOutput(ns("uiDownload"))
                  ),
                  # downloadButton(outputId = ns("Download"), label = "Download table", icon = shiny::icon("download"))),
           column(2, align = "left", offset = 10,
                  uiOutput(ns("uiFiletype"))
                  ),
           DTOutput(outputId = ns("table_output"))
         ),#for table output
      br(),
      box( height = "650", solidHeader = T, #title = "SNP Plot analysis",
          column(1, align = "right", offset = 8,
                 uiOutput(ns("UiDowloadBar"))
                 ),
          column(2, align = "left", offset = 10,
                 uiOutput(ns("uiImageType"))
                 ),
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
      updateSelectInput(inputId = "sample_name", label = "Cultivar name",
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
    gene_sample <- reactive(req(input$enter_gene))
    sample_name <- reactive(req(input$sample_name))
    type_sample <- reactive(req(input$type_name))
    impact_sample <- reactive(req(input$impact_name))
    chr_sample <- reactive(req(input$chr))

    # initialize start and end coordinate values
    start_coord <- reactiveValues(coord=NULL)
    end_coord <- reactiveValues(coord=NULL)
    # track the error: False for error msg and TRUE for no error
    track_error_df <- reactiveValues(status=NULL) # to get the error message in red and bold
    # display the error for geneId : False for error and true for no error
    # display_error <- reactiveValues(gene=NULL)
    # error msg for coordinates
    error_msg <- reactiveValues(msg=NULL)
    # error msg for geneID
    gene_error <- reactiveVal(TRUE) # NUll value doesn't react, so given TRUE - string value if error
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


    #checking for validity of geneId
    observe({
      req(gene_sample())

      gene <- reactive(unlist(strsplit(gene_sample(), "[, ]+")))  #splitting the gene on the basis of comma or space
      #gene_no <- reactive(lengths(strsplit(gene_sample(), "[, ]+")))
      #browser()
      gene_result <- lapply(gene(), check_gene, "name")
      return(gene_result)

      gene_result1 <- lapply(gene(), check_gene, "size")
      return(gene_result)
      output_gene <- as.data.frame(do.call(gene_result, gene_result1))


      #gene_result1 <- lapply(gene(), check_gene, "size")
     #browser()
      if(!isTRUE(stringr::str_detect(gene(), "TraesCS"))){
        gene_error("Invalid gene ID. Eg. TraesCS1A03G0011000")
        output$table_output <- renderDT((NULL))
        output$plot <- renderPlot(NULL)


      }else if (nchar(gene()) != 19){
        gene_error("Invalid gene ID. It must have 19 characters")
        output$table_output <- renderDT((NULL))
        output$plot <- renderPlot(NULL)

      }else{
        gene_error(TRUE) # Null value doesn't react. So, TRUE is used
      }
    })






    # feedback ----------------------------------------------------------------

    #for coordinates feedback
    observe({
     # browser()
      req(track_error_df$status, error_msg$msg)
      if(!isTRUE(track_error_df$status)){
        showFeedbackWarning(inputId="start_coord", text = error_msg$msg, color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))
         output$uiDownload <- renderUI(NULL)
         track_error_df$status <- FALSE
        # error_msg$msg <- TRUE
      } else{
        hideFeedback(inputId = "start_coord")

      }
      output$table_output <- renderDT((NULL))
      #output$UiDownload <- renderUI((NULL))
      output$uiFiletype <- renderUI((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    })

    # for checking geneId

    observe({

      req(gene_error())
      # browser()
      if(!isTRUE(gene_error())){
        showFeedbackWarning(inputId = "enter_gene", text = gene_error(), color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))

        output$uiDownload <- renderUI(NULL)
      } else{
        hideFeedback(inputId = "enter_gene")
      }
      output$table_output <- renderDT((NULL))
      #output$UiDownload <- renderUI((NULL))
      output$uiFiletype <- renderUI((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    #}
    })




    # # processing the user uploaded data in geneId-------------------------

    # observe({
    #
    # #gene_data <- reactive(input$upload)
    #
    # #browser()
    # ext <- tools::file_ext(input$upload$datapath)
    # ext2 <- tools::file_ext(input$upload$name)
    #
    # # print(ext)
    # # print(ext2)
    #
    # validate(need(ext %in% c("csv", "tsv"), "Please upload a valid file type"))
    #
    # output$table_output <- renderDT({
    #   read.csv(input$upload$datapath)
    #
    #   #file_content <- read.csv(input$upload$datapath)
    # # splitfile <- strsplit(file_content, split = "[, ]+")[[1]]
    # # for (i in 1:length(splitString)) {
    # #      cat(splitString[i])
    # #        if (splitString[i] == "[, ]+") {
    # #             cat("\n")
    # #         }
    # #    }
    #
    #   #read.tsv(input$upload$datapath)
    #  })
    #
    # })

   # processing the final table--------------------------

     final_table <- eventReactive(input$click,{
      req(sample_name())
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


      # check for error and extract the data
       query <- reactive(input$query_menu)
      #if(isTRUE(display_error$gene)) {
       #browser()
       if(query() == "geneID") {

         # query with only gene ID
         req(gene_sample())
         gene <- unlist(strsplit(gene_sample(), "[, ]+")) %>% unique() #splitting the gene on the basis of comma or space
         # gene_no <- lengths(strsplit(gene_sample(), "[, ]+"))
         #browser()
        # processing the user uploaded data in geneId
         # ext <- tools::file_ext(input$upload$datapath)
         # ext2 <- tools::file_ext(input$upload$name)
         # print(ext)
         # print(ext2)
         # validate(need(ext %in% c("csv", "tsv"), "Please upload a valid file type"))
         # read.csv(input$upload$datapath)
         # file_content <- read.csv(input$upload$datapath)
         # splitfile <- strsplit(file_content, split = "[, ]+")[[1]]
         # for (i in 1:length(splitfile)) {
         #      cat(splitfile[i])
         #        if (splitfile[i] == "[, ]+") {
         #             cat("\n")
         #         }
         #    }

         df_table <- df_sample()[df_sample()$GENE_ID %in% gene, col_list]


      # }
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

      #if(isTRUE(gene_error()) || isTRUE(track_error_df$status)) {

        if(nrow(final_table()) > 1){
          # show download button for table
          output$uiDownload <- renderUI(
            downloadButton(outputId = ns("Download"), label = "Download table", icon = shiny::icon("download"))
          )
          # show the file type
          output$uiFiletype <- renderUI(
            selectInput(inputId = ns("filetype"), label = "File Type:", choices = c("csv", "tsv", "xlsx", "xls"))
          )
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
    observe({
       req(input$upload)
      output$table_output <- renderDT({
       # browser()
        file_content <- read_delim(input$upload$datapath, col_names = "gene", delim = ",")
       # read <- readLines(input$upload$datapath)

         #split_gene <- lapply(file_content, )

        # splitfile <- strsplit(file_content$gene, split = "[, ]+")
        for (i in 1:length(splitfile)) {
          paste(splitfile[i])
          #splitfile <- strsplit(file_content$gene[i], split = "[, ]+")

          if (splitfile[i] == "[, ]+") {
            cat("\n")
          }
        }
        return(file_content)
      })

      index_gene <- which(file_content)
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
            # show download button and type here
            # output$uiDownloadBar <- renderUI(
            #   downloadButton(outputId = ns("download_bar"), label = "Download image", icon = shiny::icon("download"))
            # )
            # output$uiImageType <- renderUI(
            #   selectInput(inputId = ns("imgtype"), label = "Type:", choices = c("png", "pdf", "jpeg"))
            # )
    #       # browser()
    #       output$plot <- renderPlot(final_plot())
    #
    #         })





        # Download action---------------------------------
        # Action for table
        observe({
          # browser()
          req(is.data.frame(final_table()))
               #if(isTRUE(gene_error()) || isTRUE(track_error_df$status)) {
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
#
#                }
#           else {
#                  output$Download <- NULL
#                  content = NULL
#                }



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
  } # end of inner module server
)
        } # end of module function













#& df_sample()$TYPE == type_sample() & df_sample()$IMPACT == impact_sample(), df_sample()$GENE_ID == gene_sample()


#"TraesCS1A03G0011000"
# Tra12341A03G0011000
# TraesCS1A03G0012500
# TraesCS1A03G0011500
# TraesCS1A03G0010400

## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
