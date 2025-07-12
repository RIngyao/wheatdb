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
  type <- c(
    "intergenic_region", "upstream_gene_variant", "intron_variant", "conservative_inframe_deletion",
    "3_prime_UTR_variant", "5_prime_UTR_variant", "downstream_gene_variant", "splice_region_variant&intron_variant",
    "frameshift_variant", "conservative_inframe_insertion", "disruptive_inframe_insertion",
    "frameshift_variant&stop_gained", "frameshift_variant&splice_region_variant",
    "stop_gained&conservative_inframe_insertion", "disruptive_inframe_deletion",
    "non_coding_transcript_exon_variant", "missense_variant", "frameshift_variant&start_lost",
    "conservative_inframe_insertion&splice_region_variant",
    "splice_acceptor_variant&splice_region_variant&intron_variant",
    "splice_acceptor_variant&intron_variant", "splice_region_variant",
    "splice_donor_variant&intron_variant", "start_lost&conservative_inframe_insertion"
  )

  chrom <- paste0("Chr", rep(1:7, each = 3), rep(c("A", "B", "D"), 7))

  tagList(
    shinyFeedback::useShinyFeedback(),

    # Query Panel Section
    div(
      class = "query-section",
      style = "margin-top:10px; padding: 20px; background: #f7fafd; border: 1px solid #cce5ff; border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h3("Query Parameters", style = "color:#025b05; text-align:center; margin-bottom: 20px;"),

      fluidRow(
        column(3, selectInput(ns("query_menu"), "Query type", choices = c("None", "geneID", "type", "impact", "coordinates"))),
        column(3, selectInput(ns("variant_menu"), "Variant type", choices = c("All", "SNPs", "InDels"))),
        column(3,
               selectInput(ns("group_menu"), "Group", choices = c("All", "ILR", "IPR", "GR", "IPoR", "Durum","Sphaerococcum", "Wild"), selected = "All"),
               shinyBS::bsTooltip(id = ns("group_menu"),
                                   title = paste(
                                     "ILR - Indian Landrace",
                                     "IPR - Pre-green-revolution",
                                     "IPoR - Post-green-revolution",
                                     "GR - Four founder genotypes of green-revolution",
                                     "Sphaerococcum - Indian dwarf wheat",
                                     sep = "<br/>"
                                   ),
                                   placement = "right", options = list(container = "body"))
                 # helpText("ILR - Indian Landrace; IPR - Pre-green-revolution; IPoR - Post-green-revolution;
                 #                  GR - Four founder genotypes of green-revolution"))
               ),

        column(3, selectInput(ns("sample_name"), "Cultivar name", choices = "All", multiple = TRUE, selected = "All"))
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'geneID'", ns("query_menu")),
        fluidRow(
          column(4, radioButtons(ns("gene_choice"), "Choose", choices = c("Enter", "Upload"), inline = TRUE)),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Enter'", ns("gene_choice")),
            column(8, textInput(ns("enter_gene"), "Enter GENE ID", placeholder = "Comma or space separated"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Upload'", ns("gene_choice")),
            column(8, fileInput(ns("upload"), "Upload GENE ID")) # helpText("File must contain one gene_id per row. No header required.", style = "color:red;"))
            #column(8, helpText("File must contain one gene_id per row. No header required."))
          )
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'type'", ns("query_menu")),
        fluidRow(
          column(4, selectInput(ns("type_name"), "TYPE", choices = type)),
          column(4, selectInput(ns("chr"), "Chromosome", choices = chrom, selected = "Chr1A")),
          column(2, textInput(ns("start_coord"), "Start", placeholder = "Start value")),
          column(2, textInput(ns("end_coord"), "End", placeholder = "End value"))
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'impact'", ns("query_menu")),
        fluidRow(
          column(4, selectInput(ns("impact_name"), "IMPACT", choices = c("MODIFIER", "MODERATE", "LOW", "HIGH"))),
          column(4, selectInput(ns("chr"), "Chromosome", choices = chrom, selected = "Chr1A")),
          column(2, textInput(ns("start_coord"), "Start", placeholder = "Start value")),
          column(2, textInput(ns("end_coord"), "End", placeholder = "End value"))
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'coordinates'", ns("query_menu")),
        fluidRow(
          column(4, selectInput(ns("chr"), "Chromosome", choices = chrom, selected = "Chr1A")),
          column(4, textInput(ns("start_coord"), "Start", placeholder = "Start value")),
          column(4, textInput(ns("end_coord"), "End", placeholder = "End value"))
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] != 'None'", ns("query_menu")),
        div(style = "text-align:center; margin-top: 20px;",
            actionButton(ns("click"), "Submit", class = "btn btn-success", style = "font-weight:bold;"))
      )
    ),

    br(),

    # Table Output and Download UI
    # old--------------
    fluidRow(
      box(title = "SNP Table", height = "600px", width = 12, solidHeader = TRUE, status = "primary",
          div(style = "display: flex; justify-content: flex-end; gap: 10px; padding: 5px;",
              uiOutput(ns("uiFiletype")),
              uiOutput(ns("uiDownload"))
          ),
          DTOutput(ns("table_output"))
      )
    ),

    # Plot + Click Info
    fluidRow(
      box(title = "SNP Plot", height = "700px", width = 12, solidHeader = TRUE, status = "info",
          fluidRow(
            column(12,
                   div(style = "display: flex; justify-content: flex-end; gap: 10px; padding: 5px;",
                       uiOutput(ns("uiImageType")),
                       uiOutput(ns("UiDownloadBar"))
                   )
            )
          ),
          fluidRow(
            column(8,
                   plotOutput(ns("plot"), height = "600px", click = ns("plot_click"), hover = ns("plot_hover"))
            ),
            column(4,
                   div(
                     style = "height: 600px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #fefefe;",
                     # h4("Clicked the gene to view in JBrowse"),
                     uiOutput(ns("info_click"))
                   )
            )
          )
      )
    )
    # old----------
  )
}


#' name_of_module1 Server Functions
#'
#' @noRd
snp_table_server <- function(id) {
  moduleServer(id, function(input, output, session){

    ns <- session$ns
    # check options: All must not be together with other options--------------
    # feedback error msg for the first line of query options
    opts1_error <- reactiveVal(FALSE) # FALSE mean no error; TRUE indicate there is error
    # group_menu
    observeEvent(input$group_menu, {

      if(length(input$group_menu) > 1 && any(input$group_menu == "All")){
        showFeedbackDanger(inputId="group_menu", text = "Choose only All or exclude it from other selections",
                           color = "#ff0000", icon = shiny::icon("warning-sign", lib = "glyphicon"))
        opts1_error(TRUE)
      }else{
        hideFeedback("group_menu")
        opts1_error(FALSE)
      }
    })

    observeEvent(input$sample_name, {
      if(length(input$sample_name) > 1 && any(input$sample_name == "All")){
        showFeedbackDanger(inputId="sample_name", text = "Choose only All or exclude it from other selections",
                           color = "#ff0000") #, icon = shiny::icon("warning-sign", lib = "glyphicon"))
        opts1_error(TRUE)
      }else{
        hideFeedback("sample_name")
        opts1_error(FALSE)
      }
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


    # Create a dplyr data from DuckDB connection---------------------
    dplyr_data <- reactive({
      req(!opts1_error(), input$variant_menu, input$group_menu)
      func_extract_duckdb(var = input$variant_menu, group = input$group_menu, db = duckdb::duckdb())
    })


    column_list <- reactive(colnames(dplyr_data())) #get columns #%>% select(CHROM, POS,REF,ALT, GENE, TYPE,IMPACT, everthying()

    # update the cultivar list
    observe({
      lgt <- length(column_list())
      updated_list <- as.character(column_list()[-c(1:4,lgt,lgt-1,lgt-2)])
      updateSelectInput(inputId = "sample_name", label = "Cultivar name",
                        choices = c("All", updated_list), selected = "All")
    })


    # extracting geneId for uploaded data and manually entered data-------------------------
    gene_list <- eventReactive(input$click,{
      # browser()
      req(gene_choice())
      if(gene_choice() == "Upload"){

        path <- reactive(input$upload$datapath)
        # mention in the ui: one  gene per line# no header
        # read the genes: must be one gene per line
        gene <- reactive(readr::read_lines(path(), skip_empty_rows = TRUE))

      }else if(gene_choice() == "Enter"){
        # manually entered gene ID
        # split based on comma or space
        gene <- reactive(unlist(strsplit(gene_sample(), "[, ]+")))
      }

      return(unique(gene()))
    })


    #checking for validity of geneId-------------------------------------------
    observe({
      req(gene_list())

      # check for presence of Traes
      ge_checks <- reactive(unlist(lapply(gene_list(), check_gene, "name")))
      # check for size of gene ID
      ge_size <- reactive(unlist(lapply(gene_list(), check_gene, "size")))
      ge_end <- reactive(unlist(lapply(gene_list(), check_gene, "end")))
      # traesCS0282u82

      # if any error in the gene list: generate error message
      if(!all(ge_checks())){
        idx <- which(ge_checks() == FALSE)
        errorId <- reactive(gene_list()[idx])
        gene_error(paste0("Invalid gene ID: ", errorId(),". \nRetry again.")) #. Eg. TraesCS1A03G0011000")
        output$table_output <- renderDT((NULL))
        output$plot <- renderPlot(NULL)

      }else if(!all(ge_size())){

        idx <- which(ge_size() == FALSE)
        errorId <- reactive(gene_list()[idx])
        gene_error(paste0("Invalid gene ID: ", errorId(),". \nIt must have 19 characters.")) #. Eg. TraesCS1A03G0011000")
        output$table_output <- renderDT((NULL))
        output$plot <- renderPlot(NULL)

      } else if(!all(ge_end())){
        idx <- which(ge_end() == FALSE)
        errorId <- reactive(gene_list()[idx])
        gene_error(paste0("Invalid gene ID: ", errorId()))
        output$table_output <- renderDT((NULL))
        output$plot <- renderPlot(NULL)
      }
      else {
        gene_error(TRUE)
      }
    })

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
          output$alert <- NULL
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

         track_error_df$status <- FALSE
        # error_msg$msg <- TRUE
      } else{
        # track_error_df$status <- NULL
        hideFeedback(inputId = "start_coord")
      }
      # refresh existing
      output$uiDownload <- renderUI(NULL)
      output$uiDownloadBar <- renderUI(NULL)
      output$table_output <- renderDT((NULL))
      output$UiDownload <- renderUI((NULL))
      output$uiDownloadBar <- renderUI((NULL))
      output$uiFiletype <- renderUI((NULL))
      output$uiImageType <- renderUI((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    })

    # feedback for gene id error
    observe({

      req(gene_error())
      # gene <- reactive(unlist(strsplit(gene_sample(), "[, ]+")))  #splitting the gene on the basis of comma or space
     # print(gene)

      if(!isTRUE(gene_error())){
        showFeedbackWarning(inputId = ifelse(gene_choice() == "Enter", "enter_gene", "upload"), text = gene_error(), color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))
      } else{
        hideFeedback(inputId = ifelse(gene_choice() == "Enter", "enter_gene", "upload"))
      }

      # refresh
      output$uiDownload <- renderUI(NULL)
      output$uiDownloadBar <- renderUI(NULL)
      output$table_output <- renderDT((NULL))
      output$UiDownload <- renderUI((NULL))
      output$UiDownloadBar <- renderUI((NULL))
      output$uiFiletype <- renderUI((NULL))
      output$uiImageType <- renderUI((NULL))
      null_table$show <- TRUE
      output$plot <- renderPlot(NULL)
      null_plot$imgplot <- TRUE
    })

    # processing the final table--------------------------

    final_table <- eventReactive(input$click,{
      req(sample_name(), gene_error())
      # browser()
         if(all(sample_name() == "All")) {

           df_sample <- reactive(as.character(column_list())) #[-c(lgt,lgt-1,lgt-2)]))
           # <- reactive(col)
       }
       else{
         df_sample <- reactive(c("CHROM", "POS", "REF", "ALT", "GENE", "TYPE", "IMPACT", sample_name()))
         # df_sample <- reactive(col[,col_list])
       }



       if(query() == "geneID") {

         # query with only gene ID
         req(gene_list(), gene_choice())

         #extract data from duckdb------------------------------------------
         df_table <- dplyr_data() %>%
           filter(GENE %in% local(gene_list())) %>%
           # select relevant cultivars + required columns
           select(CHROM, POS, REF, ALT, GENE, TYPE, IMPACT, all_of(df_sample()))

         #dbGetQuery(duckdb_con(), "select * from snp_table where GENE = ?", params = list(gene_list()) ) %>% as.data.frame() %>%

    } else {

         # query with coordinates
         if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())){

           if(query() == "coordinates") {

             req(start_coord$coord(), end_coord$coord(), chr_sample())
             df_table <- dplyr_data() %>% #dbGetQuery(duckdb_con(), "SELECT *  FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ?", params = list(chr_sample(), start_coord$coord(), end_coord$coord())) %>%
               filter(CHROM %in% local(chr_sample()) &
                        (POS >= local(start_coord$coord()) & POS <= local(end_coord$coord()))
                      ) %>%
               # select only the cultivars choosen by the user
               select(CHROM,POS,REF,ALT,GENE,TYPE,IMPACT, all_of(df_sample()))

             } else if (query() == "type") {
             req(type_sample(), start_coord$coord(), end_coord$coord(), chr_sample())

             df_table <- dplyr_data() %>% #dbGetQuery(duckdb_con(), "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND TYPE = ?", params = list(chr_sample(), start_coord$coord(), end_coord$coord(), type_sample())) %>%
               filter(CHROM %in% local(chr_sample()) &
                        (POS >= local(start_coord$coord()) & POS <= local(end_coord$coord())) &
                        TYPE == local(type_sample())
                      ) %>%
               # select only the cultivars choosen by the user
               select(CHROM,POS,REF,ALT,GENE,TYPE,IMPACT, all_of(df_sample()))


           } else if (query() == "impact") {
             req(chr_sample(), impact_sample(), start_coord$coord(), end_coord$coord(), track_error_df$status)

             df_table <- dplyr_data() %>% # dbGetQuery(duckdb_con(), "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND IMPACT = ?", params = list(chr_sample(), start_coord$coord(), end_coord$coord(), impact_sample())) %>%
               filter(CHROM %in% local(chr_sample()) &
                        (POS >= local(start_coord$coord()) & POS <= local(end_coord$coord())) &
                        IMPACT == local(impact_sample())
                      ) %>%
               # select only the cultivars choosen by the user and display proper table
               select(CHROM,POS,REF,ALT,GENE,TYPE,IMPACT, all_of(df_sample()))

          }

        # }
         else {
           # Default will be null for table
           df_table <- tibble::as_tibble(NULL)
         } # end of inner if clause

       }# end of if clause

      # return and convert lazy evaluation to data.frame
       return(df_table %>% as.data.frame())
      }
   })


    # display table--------------------------------
    observe({
      req(is.data.frame(final_table()), isTRUE(gene_error()))

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

     df_plot <- reactive({
       # req(gene_sample(), gene_error())
       # req(isTRUE(gene_error()),input$click)
       req(gene_error(),input$click)

       if(query() == "geneID") {
         req(gene_sample())

         if(gene_error() && (nrow(final_table()) > 1)) {
           # browser()
           #if there is no error and there are tables
          df <- final_table() #dbGetQuery(duckdb_con(), "select * from snp_table where GENE = ?", params = list(gene))
          # print(df)
         }
       }
       else {
         # check for error and then proceed
         if(isTRUE(track_error_df$status) && isTruthy(start_coord$coord()) && isTruthy(end_coord$coord())) {
           if(nrow(final_table()) > 1) {
             if(query() == "type") {
               req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample(), type_sample())
               df <- dplyr_data() %>% #dbGetQuery(duckdb_con(), "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND TYPE = ?", params = list(chr_sample(), start_coord$coord(), end_coord$coord(), type_sample()))
                 filter(CHROM == local(chr_sample()) &
                          (POS >= local(start_coord$coord()) & POS <= local(end_coord$coord())) &
                          TYPE == local(type_sample())
                        )

             }
             else if(query() == "impact") {
               req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample(), impact_sample())
               df <- dplyr_data() %>% #dbGetQuery(duckdb_con(), "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ? AND IMPACT = ?", params = list(chr_sample(), start_coord$coord(), end_coord$coord(), impact_sample()))
                 filter(CHROM == local(chr_sample()) &
                          (POS >= local(start_coord$coord()) & POS <= local(end_coord$coord())) &
                          IMPACT == local(impact_sample())
                        )

             }
             else if(query() == "coordinates") {
               req(track_error_df$status, start_coord$coord(), end_coord$coord(), chr_sample())
               df <- dplyr_data() %>% #dbGetQuery(duckdb_con(), "SELECT * FROM snp_table  WHERE CHROM = ? AND POS>= ? AND POS<= ?", params = list(chr_sample(), start_coord$coord(), end_coord$coord()))
                 filter(CHROM == local(chr_sample()) &
                          (POS >= local(start_coord$coord()) & POS <= local(end_coord$coord()))
                        )

             }
           }

         }
         else {df <- NULL}
       }
       return(df %>% as.data.frame())

     })

      # process the graph--------------------------------------------
      final_plot <- eventReactive(input$click, {
        req(is.data.frame(df_plot()))
        # browser()
          if(nrow(df_plot()) > 1){
           # browser()
          #  print(df_plot())
            plot_info <- df_plot() %>% group_by(TYPE) %>% summarise(count = n()) %>% na.omit()
            #print(plot_info)
            data_plot <-  ggplot(data = plot_info, aes(x = TYPE, y = count, fill = TYPE)) +
              geom_bar(stat = "identity", width = 0.35)+
              coord_cartesian(clip = "off") +
              theme_classic() +
              theme(
                axis.text.x = element_text(size = 10, face = "bold", angle = 45, hjust = 1),
                axis.text.y = element_text(size = 12, face = "bold"),
                axis.title = element_text(size=12, face = "bold"),
                axis.ticks = element_line(linewidth = 2),
                legend.position = "none",
                plot.margin = margin(t=10, r=10, b=10, l=10, unit = "mm")
              ) +
              geom_text(aes(label = count), vjust = -0.6, size = 4, color = "black") +
              labs(title="Click the bar to view the list of genes",
                 x = "Type",
                 y = "No. of SNPs")

        }else{
          data_plot <- NULL
        }

        #print(data_plot)
        return(data_plot)

      })

     # display the gene along with the link to jbrowse  --------------------------------------------
     observe({
       req(input$plot_click, nrow(df_plot()) > 1, !is.null(final_plot()), input$query_menu != "None")

       if(isTRUE(gene_error())) {
         # browser()
         # if(query_menu() != "none")
         # Access plot data
         df <- df_plot()
         # process the data as used in the graph
         plot_info <- df %>% group_by(TYPE) %>% summarise(count = n()) %>% na.omit()
         plot_info$TYPE <- as.character(plot_info$TYPE)

         # Extract x position from click
         x_labels <- input$plot_click$domain$discrete_limits$x
         x_pos <- round(input$plot_click$x)

         # Check if click is within x axis bounds
         if (x_pos < 1 || x_pos > length(x_labels)) {
           output$info_click <- renderUI(NULL)
           return()
         }

         # Get the clicked TYPE label
         clicked_type <- x_labels[[x_pos]]

         # Filter df for clicked TYPE
         clicked_genes_df <- df %>%
           filter(TYPE == clicked_type) %>%
           select(GENE, CHROM, START = POS, END = POS) %>%
           distinct()

         # Get y value of click and max count for that type (to check if click is inside bar)
         y_val <- input$plot_click$y
         bar_height <- plot_info[plot_info$TYPE == clicked_type,]$count # mx bar height for the TYPE

         # Check if y click falls within bar (0 to bar_height)
         if (y_val < 0 || y_val > bar_height) {
           output$info_click <- renderUI(NULL)
           return()
         }

         # Generate UI with one JBrowse link per gene
         output$info_click <- renderUI({

           req(df_plot(), gene_data())
           if (nrow(clicked_genes_df) == 0) return(NULL)

           tagList(
             # h4("Clicked the link to view in JBrowse\n", paste0(clicked_type, ":")),
             HTML(paste0("<h4>Clicked the link to view in JBrowse<br>", clicked_type, ":</h4>")),
             lapply(seq_len(nrow(clicked_genes_df)), function(i) {
               gene <- clicked_genes_df$GENE[i]
               chr <- clicked_genes_df$CHR[i]
               start <- clicked_genes_df$START[i]
               end <- clicked_genes_df$END[i]
               # start <- ifelse(clicked_genes_df$START[i] - 100 < 1, clicked_genes_df$START[i] - 100, 1) # extend by 100 bp
               # end <- clicked_genes_df$END[i] + 100   # extend by 100bp

               jbrowse_url <- paste0(
                 "http://223.31.159.7/jb_wheatdb/?config=config.json&assembly=wheat&tracks=wheat-ReferenceSequenceTrack,gene-annotations,variants&loc=",
                 chr, ":", start, "..", end
               )

               tags$a(
                 href = jbrowse_url,
                 target = "_blank",
                 style = "display: block; margin-bottom: 4px;",
                 ifelse(clicked_type == "intergenic_region", paste0("Nearby ", gene, " (", chr, ":", start, "-", end, ")"),
                        paste0(gene, " (", chr, ":", start, "-", end, ")"))
               )
             })
           )
         })
       }

       if(isTRUE(track_error_df$status)) {
         # browser()
         # if(query_menu() != "none")
         # Access plot data
         df <- df_plot()
         # process the data as used in the graph
         plot_info <- df %>% group_by(TYPE) %>% summarise(count = n()) %>% na.omit()
         plot_info$TYPE <- as.character(plot_info$TYPE)

         # Extract x position from click
         x_labels <- input$plot_click$domain$discrete_limits$x
         x_pos <- round(input$plot_click$x)

         # Check if click is within x axis bounds
         if (x_pos < 1 || x_pos > length(x_labels)) {
           output$info_click <- renderUI(NULL)
           return()
         }

         # Get the clicked TYPE label
         clicked_type <- x_labels[[x_pos]]

         # Filter df for clicked TYPE
         clicked_genes_df <- df %>%
           filter(TYPE == clicked_type) %>%
           select(GENE, CHROM, START = POS, END = POS) %>%
           distinct()

         # Get y value of click and max count for that type (to check if click is inside bar)
         y_val <- input$plot_click$y
         bar_height <- plot_info[plot_info$TYPE == clicked_type,]$count # mx bar height for the TYPE

         # Check if y click falls within bar (0 to bar_height)
         if (y_val < 0 || y_val > bar_height) {
           output$info_click <- renderUI(NULL)
           return()
         }

         # Generate UI with one JBrowse link per gene
         output$info_click <- renderUI({

           req(df_plot(), gene_data())
           if (nrow(clicked_genes_df) == 0) return(NULL)

           tagList(
             # h4("Clicked the link to view in JBrowse\n", paste0(clicked_type, ":")),
             HTML(paste0("<h4>Clicked the link to view in JBrowse<br>", clicked_type, ":</h4>")),
             lapply(seq_len(nrow(clicked_genes_df)), function(i) {
               gene <- clicked_genes_df$GENE[i]
               chr <- clicked_genes_df$CHR[i]
               start <- clicked_genes_df$START[i]
               end <- clicked_genes_df$END[i]
               # start <- ifelse(clicked_genes_df$START[i] - 100 < 1, clicked_genes_df$START[i] - 100, 1) # extend by 100 bp
               # end <- clicked_genes_df$END[i] + 100   # extend by 100bp

               jbrowse_url <- paste0(
                 "http://223.31.159.7/jb_wheatdb/?config=config.json&assembly=wheat&tracks=wheat-ReferenceSequenceTrack,gene-annotations,variants&loc=",
                 chr, ":", start, "..", end
               )

               tags$a(
                 href = jbrowse_url,
                 target = "_blank",
                 style = "display: block; margin-bottom: 4px;",
                 ifelse(clicked_type == "intergenic_region", paste0("Nearby ", gene, " (", chr, ":", start, "-", end, ")"),
                        paste0(gene, " (", chr, ":", start, "-", end, ")"))
               )
             })
           )
         })
       }
     })

    #display the graph----------------------
    observe({
     req(!is.null(final_plot()), isTRUE(gene_error()))
    #browser()
        # show download button and type here
        output$UiDownloadBar <- renderUI(
          downloadButton(outputId = ns("download_bar"), label = "Image",
                         icon = shiny::icon("download"), class = "btn, btn-info", style="color:white",
                         title = "Download Image")
        )
        output$uiImageType <- renderUI(
          radioButtons(inputId = ns("imgtype"), inline = TRUE, label = NULL, choices = c("PNG", "PDF", "JPG"))
        )

       output$plot <- renderPlot(final_plot())

      })


    # Download action---------------------------------------------------------------------
    # Action for table
     output$Download <- downloadHandler(
       filename = function() {
         switch(input$filetype,
                "csv" = "wheatdb_snps.csv",
                "tsv" = "wheatdb_snps.tsv",
                "xlsx" = "wheatdb_snps.xlsx",
                "xls" = "wheatdb_snps.xls")
       },
       content = function(file) {
         data <- final_table()
         req(is.data.frame(data))  # Ensure data is valid

         if (input$filetype == "csv") {
           write.csv(data, file, row.names = FALSE)
         } else if (input$filetype == "tsv") {
           write.table(data, file, sep = "\t", row.names = FALSE)
         } else if (input$filetype %in% c("xlsx", "xls")) {
           # Requires the 'openxlsx' package
           openxlsx::write.xlsx(data, file)
         }
       }
     )

    # action for graph
     output$download_bar <- downloadHandler(

       filename = function() {
         # Avoid ":" in filenames (replace with "-")
         timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

         switch(input$imgtype,
                "PNG" = paste0("snps_", timestamp, ".png"),
                "PDF" = paste0("snps_", timestamp, ".pdf"),
                "JPG" = paste0("snps_", timestamp, ".jpeg"))
       },

       content = function(file) {
         req(final_plot())  # Ensure plot is available

         if (input$imgtype == "PNG") {
           png(file, width = 12, height = 8, units = "in", res = 450)
           print(final_plot())
           dev.off()
         } else if (input$imgtype == "PDF") {
           pdf(file, width = 12, height = 8, onefile = TRUE)
           print(final_plot())
           dev.off()
         } else if (input$imgtype == "JPG") {
           jpeg(file, width = 12, height = 8, units = "in", res = 450)
           print(final_plot())
           dev.off()
         }
       }
     )

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


#
## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
