#' name_of_module1 UI Function
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
                 column(8, radioButtons(inputId = ns("user_choice"), inline = TRUE, label = " ", choices = c("Enter", "Upload"))),

                    conditionalPanel(condition = sprintf("input['%s'] == 'Enter'", ns("user_choice")),
                         column(8, textAreaInput(inputId = ns("query_seq"), label = "Enter accession number(s) or FASTA sequence(s)")),
                    ),

                    conditionalPanel(condition = sprintf("input['%s'] == 'Upload'", ns("user_choice")),
                         column(8, fileInput(inputId = ns("file_blast"), label = "Or, upload file", placeholder = c("No file chosen")))
                    )
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
        column(2, selectInput(inputId = ns("max"), label = "Max target sequences", choices = c("10", "50", "100", "250", "500", "1000", "5000"))),
       # h4("Sequence of above 5 is recommended"),

        column(2, textInput(inputId = ns("exp_thres"), label = "Expect threshold")),
        column(2, selectInput(inputId = ns("word_size"), label = "Word size", choices = c("16", "20", "24", "28", "32", "48", "64"))),
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
        #column(6, selectInput(inputId = ns("score"), label = "Match/Mismatch Scores", choices = c("10", "50", "100", "250", "500", "1000", "5000"))),
       #column(6, textInput(inputId = ns("gap_c"), label = "Gap cost")),
        column(2, textInput(inputId = ns("gap_open"), label = "Gap opening")), #cost to open a gap
        column(2, textInput(inputId = ns("gap_ext"), label = "Gap extension")), #cost to extend a gap
        column(2, selectInput(inputId = ns("rewards"), label = "Rewards", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                                                      "11", "12", "13", "14", "15"))),  #penalty for a nucleotide mismatch
        column(2, selectInput(inputId = ns("pen"), label = "Penalty", choices = c("-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "-9", "-10",
                                                                                 "-11", "-12", "-13", "-14", "-15")))  # reward for a nucleotide match
        )
    ),

    actionButton(inputId = ns("click"), label = "BLAST", width = "10%", class = "btn-info btn-sm", style = "font-weight:bold; align: center;"),
    uiOutput(outputId = ns("blast_out")),  #give the blast results here

    #TableOutput(outputId = ns("blast_table"))
     DTOutput(outputId = ns("blast_table")),
     verbatimTextOutput(outputId = ns("display"))

    # hidden(div(id = "loading",
    #        tags$strong("BLAST is loading......")))

  )
}

#' name_of_module1 Server Functions
#'
#' @noRd
 blast_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  #setting the default values to blast parameters
    default_val <- list(max = 10, exp_thres = 0.05, word_size = 11, match_sc = 1, mismatch_sc = -3, gap_open = 5, gap_ext = 2, penalty = 0, reward = 0)


    observe({

    target <- reactive(
      if(purr::is_empty(input$max)) {
        input <- NULL
      } else {
        input <- input$max
      })
    wordlen <- reactive(
      if(purr::is_empty(input$size)) {
        input <- NULL
      } else {
        input <- input$size
      })
    eval <- reactive(
      if(purr::is_empty(input$exp_thres)) {
        input <- NULL
      } else {
        input <- input$exp_thres
      })

   })
    file_upload <- reactive(input$upload)
    query <- reactive(input$query_seq)
    file_error <- reactiveValues(status=NULL)
    error_msg <- reactiveValues(msg=NULL)


    choice <- reactive(input$user_choice)



    #checking for validity of the fasta sewuence-------------------------------------

      valid_file <- reactive({
      result <- sapply(input$user_choice, function(x) stringr::str_detect(x, "A|C|G|T|U|R|Y|S|W|K|M|B|D|H|V|N|.|-"))
      return(result)
 })

    observe({
      if(!isTruthy(valid_file())) {
        file_error$status <- reactive(FALSE)
        error_msg$msg <- reactive("Please enter a valid file!")
        output$blast_table <- NULL
      }
    })



    # show feedback--------------------------------------
    observe({
      # browser()
      req(file_error$status, error_msg$msg)
      if(!isTRUE(file_error$status)){
        showFeedbackWarning(inputId="query_seq", text = error_msg$msg, color = "#ff0000",
                            icon = shiny::icon("warning-sign", lib = "glyphicon"))

        file_error$status <- FALSE
        output$blast_table <- NULL
      } else{
        hideFeedback(inputId = "query_seq")
      }
    })

    # processing the type of input given by the user-----------------------------------------
    blast_in <- eventReactive(input$click,  {
      req(user_choice(), in_query())

    # Check for manually entered data
      if(user_choice == "Enter") {

        #manually enter the fasta file
         sequence <- input$query_seq


       } else if(user_choice == "Upload") {


         # check the file is empty or not
         if(is.null(file_upload()) || file_upload()$size == 0) {
           file_error$status <- TRUE
         }

          else if(isTRUE(valid_file()) && (!is.null(file_upload())) ) {  #check for file validity and file is not empty

           # upload the file
           sequence <- readDNAStringSet("run/media/data3/user1/ref_data/iwgsc_refseqv2.1_assembly.fa.gz", format = "fasta")
          }
         }

      })


        #checking for valid parameters---------------------------------
         observe({

           #for max target sequence
           target_check <- max_target(input$max, 10)


           #for gap open and extend
           gap_opn <- reactive(gap_op(input$gap_open, 5))
           gap_ex <- reactive(gap_ext(input$gap_ext, 2))

           #for word size
           word_size <- word_size(input$wordlen, 11)
           #for match/mismatch score
            pos <- reactive(reward(input$rewards, 2))
            neg <- reactive(penalty(input$pen, -3))


           #for e-value score
           evalue <- e_value(input$eval, 0.05)
         })


      # get all the parameters
      # if not provided, use default values
      # for required parameters - provide alert msg.

    # performing the blastn-----------------------------------------------------

      #set parameters


   observeEvent(input$click, {
    # browser()

     # get the user query
     # if the user provide a sequence, instead of a file
    #
     if(choice() == "Enter"){
       user_seq <- "TTCACGGTTCAGAATTTATATTGCGGGACGTTCGACCTCCAATTACATGTTTTAA"
       query <- paste(">query",user_seq, sep = "\n")
       write_lines(query,"iwgsc_refseqv2.1_assembly.fa.gz")
     }else if(choice() == "Upload"){

       # import the file using Biostring
         filepath <- system.file("extdata", input$file_blast, package = "Biostrings")

         seqs <- readDNAStringSet(filepath)


        # and save it to user_sequence.fa
        writeXStringSet(seqs, filepath = "iwgsc_refseqv2.1_assembly.fa.gz")

     }

     # save the sequence as file
     query <- "test.fa"

    # browser()

     e_val <- e_value(input$val, 0.001)
     word_size_val <- word_size(input$wordlen, 11)
     gap_op_val <- gap_op(input$gap_open, 5)
     gap_ext_val <- gap_ext(input$gap_ext, 2)
     reward_val <- reward(input$rewards, 1)
     penalty_val <- penalty(input$pen, -2)
     max_target_val <- max_target(input$max, 10)

     future::plan(multisession, workers = 10)

     future({
       # run the blast asynchronously
       blastcmd <- sprintf("nohup ./run_blast.sh %s %f %.0f %.0f %.0f %.0f %.0f %.0f", query, e_val, word_size_val, gap_op_val,
                           gap_ext_val, reward_val,penalty_val, max_target_val)

       system(blastcmd, wait = TRUE)

     }) %...>% # promises
       {
         progressSweetAlert(
           session=session,
           id = "myprogress",
           title = "BLAST is running.....",
           display_pct = TRUE,
           value = 0
         )
         for (i in seq_len(50)) {
           Sys.sleep(0.1)
           updateProgressBar(
             session = session,
             id = "myprogress",
             value = i*2
           )
         }

         closeSweetAlert(session = session)

         sendSweetAlert(
           session = session,
           title ="BLAST results done!", style = "width:50px;",
           type = "success",
         )


          blast_output <- vroom::vroom("blast_result.out", delim = "\t", col_names = c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore"))


          observe({
            req(query())
            output$blast_table <- renderDT({
              datatable(cbind(blast_output),
                        options = list(
                          scrollX = TRUE,
                          scrollY = "250px"
                        ))

            })
          })

          observe({
            req(query())
            output$display <- renderText(paste(
              "qseqid: query or source (gene) sequence id;",
              "sseqid: subject or target (reference genome) sequence id;",
              "pident: percentage of identical positions;",
              "length: alignment length (sequence overlap);",
              "mismatch: number of mismatches;",
              "gapopen: number of gap openings;",
              "qstart: start of alignment in query;",
              "qend: end of alignment in query;",
              "sstart: start of alignment in subject;",
              "send: end of alignment in subject;",
              "evalue: expect value;",
              "bitscore: bit score",
              sep = "\n"))
          })


       } %...!% # promise error
       {
         function(e) {
           progressSweetAlert(
             session=session, id = "myprogress",
             title = "BLAST is running.....",
             display_pct = TRUE,
             value = 0
           )
           for (i in seq_len(50)) {
             Sys.sleep(0.1)
             updateProgressBar(
               session = session,
               id = "myprogress",
               value = i*2
             )
           }

           closeSweetAlert(session = session)

           output$blast_out <- renderUI(paste("Error in BLAST result", e$msg))
         }
       }




   })




  })
 }

 # TTCACGGTTCAGAATTTATATTGCGGGACGTTCGACCTCCAATTACATGTTTTAA

## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
