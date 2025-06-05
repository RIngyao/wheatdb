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
  # important variables-------------------------------------------------------------------------------------------------------------------------------
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
  # start ui-------------------------------------------------------------------------------------------------------------------------------------------
  tagList(
    shinyFeedback::useShinyFeedback(),
    # box for query parameters--------------------------------------------------
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
          column(6,selectInput(inputId = ns("query_menu"), label = "Query type", choices = c("None", "geneID", "type", "impact", "coordinates"))),
          column(6,selectInput(inputId = ns("sample_name"), label = "Cultivar name", choices = c("All"), selected = "All"))
        )
      ),

      div(
        style = "display:flex; justify-content:center; padding: 5px",

        conditionalPanel(condition = sprintf("input['%s'] == 'geneID'", ns("query_menu")),

                         # change the button type: use raditobutton
                         fluidRow(
                           column(4, radioButtons(inputId = ns("gene_choice"), inline = TRUE, label = "Choose", choices = c("Enter", "Upload"))),

                          conditionalPanel(condition = sprintf("input['%s'] == 'Enter'", ns("gene_choice")),
                                           column(8,textInput(inputId = ns("enter_gene"), label = "Enter GENE ID", placeholder = c("Must be comma or space separated!")))
                                           ),

                          conditionalPanel(condition = sprintf("input['%s'] == 'Upload'", ns("gene_choice")),
                                             column(8,fileInput(inputId = ns("upload"), label = "Upload GENE ID", placeholder = "No files selected" ))
                                             )

                         )

                         ), # end of conditionpanel geneID

        # ui for type-----------------------------------------------------------
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

        # ui for coorindates ---------------------------------------------------
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

      # button for submitting the query-----------------------------------------
      conditionalPanel(condition = sprintf("input['%s'] != 'None'", ns("query_menu")),
                       actionButton(inputId = ns("click"), label = "SUBMIT",
                                    width = "10%",
                                    # class = "btn-primary btn-lg",
                                    class = "btn-info btn-sm",
                                    style = "font-weight:bold;")
                       )

    ),


    # display the download button only when the data is ready (table and figure)----------------
     fluidRow(
      box( height = "600", solidHeader = T, #title = "SNP Table",
       div(
         style = "display: flex; justify-content: flex-end; gap: 15px; padding: 10px;",
         uiOutput(ns("uiFiletype")),
         uiOutput(ns("uiDownload"))
       ),

           DTOutput(outputId = ns("table_output"))
         ),#for table output
      br(),
      box( height = "650", solidHeader = T, #title = "SNP Plot analysis",

           div(
             style = "display: flex; justify-content: flex-end; gap: 15px; padding: 10px;",
             uiOutput(ns("uiImageType")),
             uiOutput(ns("UiDownloadBar"))
           ),
        # column(12,
        #        column(8, align = "left", offset = 0, uiOutput(ns("uiImageType"))),
        #        column(2, align = "right", offset = 10, uiOutput(ns("UiDownloadBar")))
        # ),

          column(12,
              div(height=600, width = 600,
                    plotOutput(outputId = ns("plot"), click = ns("plot_click"), hover= ns("plot_hover")))
                ),
          column(12,
                 div(height=300, width = 300,
                    # verbatimTextOutput(outputId = ns("info_click")))),
                    uiOutput(ns("info_click"))
                    )
                 ),
          column(12,
                 div(height=300, width = 300,
                     verbatimTextOutput(outputId = ns("info_hover")))),
          column(12,
                 div(height = 300, width = 300,
                     htmlOutput(outputId = ns("show_link"))))

      ),

    )
)
}

#' name_of_module1 Server Functions
#'
#' @noRd
snp_table_server <- function(id) {
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # connect duckdb
    con <- dbConnect(duckdb::duckdb(), dbdir = "data-raw/final.duckdb")

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
    gene_choice <- reactive(req(input$gene_choice))
    gene_sample <- reactive(req(input$enter_gene))
    query <- reactive(req(input$query_menu))
    file_upload <- reactive(req(input$upload))
    sample_name <- reactive(req(input$sample_name))
    type_sample <- reactive(req(input$type_name))
    impact_sample <- reactive(req(input$impact_name))
    chr_sample <- reactive(req(input$chr))
    gene_data <- reactive(req(input$plot_click))

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
             output$alert <-

               (NULL)
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




    # feedback ----------------------------------------------------------------

    #for coordinates feedback
    observe({
     # browser()
      req(track_error_df$status, error_msg$msg)
      if(!isTRUE(track_error_df$status)){
        showFeedbackWarning(inputId="start_coord", text = error_msg$msg, color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))
         output$uiDownload <- renderUI(NULL)
         output$uiDownloadBar <- renderUI(NULL)
         track_error_df$status <- FALSE
        # error_msg$msg <- TRUE
      } else{
        hideFeedback(inputId = "start_coord")

      }
      output$table_output <- renderDT((NULL))
      output$UiDownload <- renderUI((NULL))
      output$uiDownloadBar <- renderUI((NULL))
      output$uiFiletype <- renderUI((NULL))
      output$uiImageType <- renderUI((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    })


    observe({

      req(gene_error())
       #browser()
      gene <- reactive(unlist(strsplit(gene_sample(), "[, ]+")))  #splitting the gene on the basis of comma or space
     # print(gene)

      if(!isTRUE(gene_error())){
        showFeedbackWarning(inputId = "enter_gene", text = gene_error(), color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))

        output$uiDownload <- renderUI(NULL)
        output$uiDownloadBar <- renderUI(NULL)
      } else{
        hideFeedback(inputId = "enter_gene")
      }
      output$table_output <- renderDT((NULL))
      output$UiDownload <- renderUI((NULL))
      output$UiDownloadBar <- renderUI((NULL))
      output$uiFiletype <- renderUI((NULL))
      output$uiImageType <- renderUI((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    #}
    })

    observe({
      req(gene_error())
      if(!isTRUE(gene_error())){
        showFeedback(inputId = "upload", text = gene_error())

        output$uiDownload <- renderUI(NULL)
        output$uiDownloadBar <- renderUI(NULL)
      } else{
        hideFeedback(inputId = "upload")
      }
      output$table_output <- renderDT((NULL))
      output$UiDownload <- renderUI((NULL))
      output$UiDownloadBar <- renderUI((NULL))
      output$uiFiletype <- renderUI((NULL))
      output$uiImageType <- renderUI((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    })



    # processing geneId for uploaded data and manually entered data-------------------------
    gene_list <- eventReactive(input$click,{
     # browser()
      req(gene_choice())
      if(gene_choice() == "Upload"){
        path <- reactive(input$upload$datapath)

        # get the file extension
        ext <- reactive(tools::file_ext(input$upload$name))
        print(str(ext()))

        if(!isTRUE(ext() %in% c("csv", "tsv"))) {
        gene_error(paste0("Please upload a valid file type"))
        }
         else {
        gene_error(TRUE)
        }

        # load the data
        gene <- reactive(
          switch(ext(),
                 "csv" = read_csv(path()),
                 "tsv" = read_tsv(path())
          )
        )

       }

      req(gene_choice(), gene_sample())
        if(gene_choice() == "Enter"){
        # manually entered gene ID

        # split based on comma or space
        gene <- reactive(unlist(strsplit(gene_sample(), "[, ]+")))

      }

      return(gene())
    })


    #checking for validity of geneId-------------
    observe({
      req(gene_list())
     # browser()
      # check for presence of TraesCS

    # if(length(gene_error()) ==1 && isTRUE(gene_error())){
        ge_checks <- reactive(unlist(lapply(gene_list(), check_gene, "name")))
        ge_size <- reactive(unlist(lapply(gene_list(), check_gene, "size")))


      if(!all(ge_checks())){
        idx <- which(ge_checks() == FALSE)
         #print(idx)
        errorId <- reactive(gene_list()[idx])
        # print(errorId())
        gene_error(paste0("Invalid gene ID: ", errorId(),". \nRetry again.")) #. Eg. TraesCS1A03G0011000")
       # print(gene_error())
        output$table_output <- renderDT((NULL))
        output$plot <- renderPlot(NULL)

      }else if(!all(ge_size())){

          idx <- which(ge_size() == FALSE)
          errorId <- reactive(gene_list()[idx])
          gene_error(paste0("Invalid gene ID: ", errorId(),". \nIt must have 19 characters.")) #. Eg. TraesCS1A03G0011000")
          output$table_output <- renderDT((NULL))
          output$plot <- renderPlot(NULL)

      }
        else {
          gene_error(TRUE)
        }
    })

   # processing the final table--------------------------

     final_table <- eventReactive(input$click,{
      req(sample_name())
       # browser()
       col <- dbGetQuery(con, "desc snp_table")
       if(sample_name() == "All") {
         lgt <- length(col$column_name)
          col_list <- as.character(col$column_name[-c(1:4,lgt,lgt-1,lgt-2)])
       }
       # else {
       #  col_list <-
       #
       # }



       # first arrange proper column name
       # if(sample_name() == "All") {
       #   col_list <- colnames(snps_df)
       #   df_sample <- reactive(snps_df)
       # }
       # else{
       #   col_list <- c("#CHROM", "POS", "REF", "ALT", "TYPE", "IMPACT", "GENE", sample_name())
       #   df_sample <- reactive(snps_df[,col_list])
       # }


      # check for error and extract the data

       if(query() == "geneID") {

         # query with only gene ID
         req(gene_sample())
         gene <- unlist(strsplit(gene_sample(), "[, ]+")) %>% unique() #splitting the gene on the basis of comma or space

         #extract data from duckdb------------------------------------------
         df_table <- dbGetQuery(con, "select * from snp_table where GENE = ?", params = list(gene) ) %>% as.data.frame()

       }
       else{

         # query with coordinates
         if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())){

           if(query() == "coordinates") {

             req(start_coord$coord(), end_coord$coord(), chr_sample())
             df_table <- dbGetQuery(con, "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ?", params = list(chr_sample(), start_coord(), end_coord()))

             }
           else if(query() == "type") {
             req(type_sample(), start_coord$coord(), end_coord$coord(), chr_sample())
             df_table <- dbGetQuery(con, "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND TYPE = ?", params = list(chr_sample(), start_coord(), end_coord(), type_sample()))

           }
           else if(query() == "impact") {
             req(chr_sample(), impact_sample(), start_coord$coord(), end_coord$coord(), track_error_df$status)
             df_table <- dbGetQuery(con, "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND IMPACT = ?", params = list(chr_sample(), start_coord(), end_coord(), impact_sample()))

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
            downloadButton(outputId = ns("Download"), label = "Table",
                           icon = shiny::icon("download"), class = "btn btn-info",
                           style = "color:white", title = "Download table")
          )
          # show the file type
          output$uiFiletype <- renderUI(
            radioButtons(inputId = ns("filetype"), inline = TRUE, label = NULL, choices = c("csv", "tsv", "xlsx"))
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



     gene <- reactive(unlist(strsplit(gene_sample(), "[, ]+")) %>% unique()) #splitting the gene on the basis of comma or space

     # data for graph------------
     # df_plot <- eventReactive(input$click,{
     df_plot <- reactive({
       # req(gene_sample(), gene_error())
       req(gene_error(), input$click)
       # browser()
       if(query() == "geneID") {

         req(gene_sample())
         if(isTRUE(gene_error()) && (nrow(final_table()) > 1)) {
          df <- final_table() #dbGetQuery(con, "select * from snp_table where GENE = ?", params = list(gene))

         }

       }
       else {
         # check for error and then proceed
         if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())) {
           if(nrow(final_table()) > 1) {
             if(query() == "type") {
               req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample(), type_sample())
               df <- dbGetQuery(con, "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND TYPE = ?", params = list(chr_sample(), start_coord(), end_coord(), type_sample()))

             }
             else if(query() == "impact") {
               req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample(), impact_sample())
               df <- dbGetQuery(con, "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND IMPACT = ?", params = list(chr_sample(), start_coord(), end_coord(), impact_sample()))

             }
             else if(query() == "coordinates") {
               req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample())
               df <- dbGetQuery(con, "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ?", params = list(chr_sample(), start_coord(), end_coord()))

             }
           }

         }
         else {df <- NULL}
       }
       return(df)

     })

      # process the graph--------------------------------------------
      final_plot <- eventReactive(input$click, {
        req(is.data.frame(df_plot()))
       #  browser()
         print(df_plot())
          if(nrow(df_plot()) > 1){
           # browser()
          #  print(df_plot())
            plot_info <- df_plot() %>% group_by(TYPE) %>% summarize(count = n(), GENE = unique(GENE))
            #print(plot_info)
            data_plot <-  ggplot(data = plot_info, aes(x = TYPE, y = count, fill = TYPE)) +
            geom_bar(stat = "identity", width = 0.35)+
            theme_classic() +
            theme(
              axis.text.x = element_text(size = 10, face = "bold", angle = 45, hjust = 1),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title = element_text(size=12, face = "bold"),
              axis.ticks = element_line(linewidth = 2),
              legend.position = "none"
            ) +
              geom_text(aes(label = count), vjust = -0.6, size = 4, color = "black") +
            labs(title="Number of genes overlap with the SNPs. It can be either within genic or intergenic regions",
                 x = "Type",
                 y = "count")



        }else{
          data_plot <- NULL
        }

        #print(data_plot)
        return(data_plot)

      })

     # subset of df_plot--------------------------------------------
     # browser()



     observe({
       req(input$plot_click, df_plot(), input$click)

       # browser()
       clicked <- reactive(input$plot_click$domain$discrete_limits$x)
       #browser()
       x_pos <- reactive(round(input$plot_click$x))
       y_pos <- reactive(round(input$plot_click$y))
        print(y_pos())
       # req(df_plot())
      # browser()
      # print(str(clicked()))
      # print(clicked())
      #  print(input$plot_click$x)
      #  print(input$plot_click$y)

       # process it
       plot_info_click <- df_plot() %>% group_by(TYPE) %>% summarize(count = n(), GENE = unique(GENE))
       printed_gene <- reactive(unique(plot_info_click$GENE[plot_info_click$TYPE %in% clicked()]))
       print(printed_gene())
     #  gene_selected <- reactive(plot_info_click[plot_info_click$TYPE == clicked()]$GENE %>% unique())
       click_type_x <- reactive(input$plot_click$domain$discrete_limits$x[x_pos()])

       type_c <- plot_info_click[plot_info_click$TYPE == click_type_x(), "count", drop = TRUE]
       print(type_c)


       if(all(y_pos() >= 0) & all(y_pos() <= type_c)){

         #   output$info_click <- renderPrint({
         #     print(printed_gene())
         # }) unix

      # browser()


        output$info_click <- renderUI({
          print(printed_gene())
          tags$a(href = "https://www.google.co.in/",
                 target = "_blank",
                 paste(printed_gene()),


          # # defining custom genomes with data provided by URLs-------------------------
          # base_url <- "https://gladki.pl/igvr/testFiles"
          # title <- "ribo remote"
          # fasta_file <- sprintf("%s/%s", base_url, "ribosomal-RNA-gene.fasta")
          # fasta_index_file <- sprintf("%s/%s", base_url, "ribosomal-RNA-gene.fasta.fai")
          # annotation_file <- sprintf("%s/%s", base_url, "ribosomal-RNA-gene.gff3")
          # locus <- "U13369.1:7,276-8,225"
          # genomeOptions <- parseAndValidateGenomeSpec(
          #   genomeName = "hg38",
          #   initialLocus = "NDUFS2",
          #   dataMode = "http",
          #   stockGenome = FALSE,
          #   fasta = fasta_file,
          #   fastaIndex = fasta_index_file,
          #   genomeAnnotation = annotation_file
          #
          # )
          # genomeOptions


#
# # defining custom genomes with data provided on local files------------------
# data_directory <- system.file(package = "igvShiny", "extdata"),
# fasta_file <- file.path(data_directory, "ribosomal-RNA-gene.fasta"),
# fasta_index_file <- file.path(data_directory, "ribosomal-RNA-gene.fasta.fai"),
# annotation_file <- file.path(data_directory, "ribosomal-RNA-gene.gff3"),
# genomeOptions2 <- parseAndValidateGenomeSpec(
#   genomeName = "ribo local",
#   initialLocus = "U13369.1:7,276-8,225",
#   dataMode = "localFiles",
#   stockGenome = FALSE,
#   fasta = fasta_file,
#   fastaIndex = fasta_index_file,
#   genomeAnnotation = annotation_file
# ),

)
          })

       } else {
         output$info_click <- NULL
       }



     })

       observe({
         req(input$plot_hover, df_plot(), input$click,
             is.numeric(input$plot_hover$x), is.numeric(input$plot_hover$y))

         # data for display
         plot_info_hover <- df_plot() %>% group_by(TYPE) %>% summarise(count = n(), unique_count = n_distinct(GENE))
         # print(plot_info_hover)
         count_type <- reactive(df_plot() %>% group_by(TYPE) %>% summarise(count = n()))


        #  browser()
        # print(input$plot_hover)
          # get the x-axis position
        # print(input$plot_hover$y)
        # print(input$plot_hover$x)

         x_pos <- reactive(round(as.numeric(input$plot_hover$x)))
         y_pos <- reactive(round(as.numeric(input$plot_hover$y)))
         # rf <- class(x_pos)
         # print (rf)

         print(str(x_pos()))
         print(str(y_pos()))

          # extract the type for the x-position
          # req(x_pos() %in% c(1:10))
         hover_type_x <- reactive(input$plot_hover$domain$discrete_limits$x[x_pos()])
         # count of gene for the type
         hover_df <- plot_info_hover %>% filter(TYPE == hover_type_x()) # && (plot_info_hover %>% filter(count == y_pos()))
         hover_df_y <- plot_info_hover %>% filter(count == y_pos())
         gene_count <- reactive(sum(hover_df$unique_count))
         type_c <- plot_info_hover[plot_info_hover$TYPE == hover_type_x(), "count", drop = TRUE]
         print(type_c)
          # req(y_pos() <= type_c)


         if(y_pos() >= 0 && y_pos() <= type_c){

           output$info_hover <- renderPrint({
             print(paste(hover_type_x(), ": unique gene count = ", gene_count()))


           })

         }
          else {
            output$info_hover <- NULL
          }


  })

        #display the graph
        observe({
         req(!is.null(final_plot()))
        #browser()
            # show download button and type here
            output$UiDownloadBar <- renderUI(
              downloadButton(outputId = ns("download_bar"), label = "Image",
                             icon = shiny::icon("download"), class = "btn, btn-info", style="color:white",
                             title = "Download Image")
            )
            output$uiImageType <- renderUI(
              radioButtons(inputId = ns("imgtype"), inline = TRUE, label = NULL, choices = c("png", "pdf", "jpeg"))
            )

           output$plot <- renderPlot(final_plot())

          })





        # Download action---------------------------------------------------------------------
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

        })
#
#
  } # end of inner module server

   )      }# end of module function










# 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
# @jbrowse/cli/node_modules/@oclif/core/lib/command.js
#'/usr/lib/node_modules/@jbrowse/cli/node_modules/@oclif/core/lib/command.js'
# /node_modules/@oclif/core/lib/command.js:45



#"TraesCS1A03G0011000"
# Tra12341A03G0011000
# TraesCS1A03G0012500
# TraesCS1A03G0011500
# TraesCS1A03G0012700
# TraesCS1A03G0010400
# TraesCS1A03G0009800
# TraesCS1A03G0007600
# TraesCS1A03G0005200
# TraesCS1A03G0003200



## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
