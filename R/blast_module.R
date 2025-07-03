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
    shinyFeedback::useShinyFeedback(),

    # FASTA input section
    div(
      class = "card p-3 mb-3",
      style = "background: linear-gradient(to bottom right, #ffffff, #d2e2df); box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
      tags$h3("BLASTn Query", class = "text-success", style = "font-weight:bold"),

      radioButtons(ns("user_choice"), label = NULL, choices = c("Enter", "Upload"), inline = TRUE),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'Enter'", ns("user_choice")),
        textAreaInput(ns("query_seq"), label = "Input FASTA sequence(s)",
                      placeholder = "Paste sequence(s) here", height = "150px", width = "100%")
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'Upload'", ns("user_choice")),
        fileInput(ns("file_blast"), label = "Upload FASTA File", placeholder = "Choose a file")
      )
    ),

    # General Parameters
    div(
      class = "card p-3 mb-3",
      style = "background: linear-gradient(to bottom right, #ffffff, #d2e2df); box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
      tags$h3("General Parameters", class = "text-success"),

      fluidRow(
        column(4, selectInput(ns("max"), "Max target sequences", choices = c("10", "50", "100", "250", "500", "1000", "5000"), selected = "100")),
        column(4, textInput(ns("exp_thres"), "Expect threshold", placeholder = "e.g., 0.05")),
        column(4, numericInput(ns("wordlen"), "Word size", min = 4, max = 50, value = 11, step = 1))
      )
    ),

    # Scoring Parameters
    div(
      class = "card p-3 mb-3",
      style = "background: linear-gradient(to bottom right, #ffffff, #d2e2df); box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
      tags$h3("Scoring Parameters", class = "text-success"),

      fluidRow(
        column(3, textInput(ns("gap_open"), "Gap opening", placeholder = "e.g., 5")),
        column(3, textInput(ns("gap_ext"), "Gap extension", placeholder = "e.g., 2")),
        column(3, selectInput(ns("rewards"), "Match Reward", choices = as.character(1:15), selected = "2")),
        column(3, selectInput(ns("pen"), "Mismatch Penalty", choices = as.character(-1:-15), selected = "-3"))
      )
    ),

    # Run BLAST
    div(
      style = "text-align:center;",
      actionButton(ns("click"), "Run BLAST", class = "btn btn-success", style = "font-weight:bold; width: 180px;")
    ),

    # Output section
    div(
      class = "mt-4",

      #
      uiOutput(ns("blast_msg"), class = "alert alert-info", style = "margin-top:20px; font-size:16px; font-weight:500;"),

      div(
        style = "margin-top: 20px;",
        DTOutput(ns("blast_table"))
      ),

      div(
        # style = "margin-top:10px; max-height:150px; overflow-y:auto; border: 1px solid #ccc; background-color: #f8f9fa; padding: 10px;",
        verbatimTextOutput(ns("display"))
      )
    )
  )
}





#' name_of_module1 Server Functions
#'
#' @noRd
 blast_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # user's choice----------
    choice <- reactive(input$user_choice)
    error_sign <- reactiveVal(FALSE) # FALSE - no error; TRUE - there is error signal
    error_msg <- reactiveVal()
    # Check the validity and get the input sequence provided by the user---------------------

    query_seq <- eventReactive(input$click,{

      if(choice() == "Enter"){ # paste sequence
        paste_seq <- req(input$query_seq)
        # Split lines
        lines <- strsplit(paste_seq, "\\n")[[1]]
      } else if(choice() == "Upload"){
        file <- req(input$file_blast)
        lines <- readLines(file$datapath, warn = FALSE)
      }

      # trim whitespace and remove empty lines
      lines <- trimws(lines)
      lines <- lines[lines != ""]

      # Identify header lines and their indices
      header_indices <- grep("^>", lines)

      if(length(header_indices) == 0){
        # if no header and multiple seq, consider single unnamed
        # combine the seq
        seq_combined <- paste(lines, collapse = "")
        # check
        if(!grepl("^[ATGCNatgcn]+$", seq_combined)){
          error_sign(TRUE)
          error_msg("Error: invalid input sequence!")
          return(NULL)
        }else{
          # add header and return the valid seq
          valid_fa <- paste0(">querySeq\n", seq_combined)
          return(valid_fa)
          # print(valid_fa)
        }

        # end of no header sequence
      } else {

        # with header
        # chec all headers are correctly formated
        if(any(!grepl("^>", lines[header_indices]))){
          error_sign(TRUE)
          error_msg("Error: invalid header! ", lines[header_indices])
          return(NULL)
        }

        # check sequence
        seq_valid <- TRUE
        for(i in seq_along(header_indices)){
          start_idx <- header_indices[i] + 1
          end_idx <- if(i == length(header_indices)) length(lines) else header_indices[i+1] - 1
          if(start_idx <= end_idx){
            seq_block <- paste(lines[start_idx:end_idx], collapse = "")
            if(!grepl("^[ATGCNatgcn]+$", seq_block)){
              error_sign(TRUE)
              error_msg(paste0("Error: invalid sequence detected after header ", lines[header_indices[i]]))
              seq_valid <- FALSE
              break
            }
          } else {
            # No sequence lines after header — possibly empty sequence
            error_sign(TRUE)
            error_msg(paste0("Error: empty sequence after header ", lines[header_indices[i]]))
            seq_valid <- FALSE
            break
          }
        }

        if(!seq_valid) return(NULL)

        # If all sequences valid, reconstruct FASTA text and return
        valid_fa <- paste(lines, collapse = "\n")
        return(valid_fa)
      } # end with header
    })


   # performing the blastn-----------------------------------------------------
    observeEvent(input$click, {

      req(query_seq())

      #browser()
      # Save query sequence as FASTA file
      query_path <- "data-raw/query.fa"
      write_lines(query_seq(), query_path)


      # Collect validated parameters
      params <- list(
        e_val         = e_value(input$val, 0.05),
        word_size     = word_size(input$wordlen, 11),
        gap_open      = gap_op(input$gap_open, 5),
        gap_extend    = gap_ext(input$gap_ext, 2),
        reward        = reward(input$rewards, 1),
        penalty       = penalty(input$pen, -2),
        max_targets   = max_target(input$max, 10)
      )

      # Show status message that BLAST started
      output$blast_msg <- renderText("BLAST is running...")
      future::plan(multisession, workers = 1)

      # Run BLAST asynchronously
      #browser()
      future({

        blastcmd <- sprintf(
          "nohup ./run_blast.sh %s %f %.0f %.0f %.0f %.0f %.0f %.0f",
          query_path,
          params$e_val, params$word_size,
          params$gap_open, params$gap_extend,
          params$reward, params$penalty,
          params$max_targets
        )

        system(blastcmd, wait = TRUE)
      }) %...>% {

         # Read BLAST output # blast successfully completed
        blast_output <- vroom::vroom("data-raw/blast_result",
                                     delim = "\t",
                                     col_names = c("qseqid", "sseqid", "pident", "length",
                                                   "mismatch", "gapopen", "qstart", "qend",
                                                   "sstart", "send", "evalue", "bitscore"))
       # last_msg <- renderText("BLAST completed successfully!")
        output$blast_msg <- renderText("BLAST completed successfully!")

          # Update status to done
       #   output$b
        #  browser()


          # Render the results table
          output$blast_table <- renderDT({
            datatable(blast_output,
                      options = list(scrollX = TRUE, scrollY = "600px",
                                     scrollCollapse = TRUE),
                      rownames = FALSE)
          })

          # Render the description text
          output$display <- renderText(paste(
            "qseqid: Query (gene) sequence ID",
            "sseqid: Subject (reference) sequence ID",
            "pident: % identity",
            "length: Alignment length",
            "mismatch: Number of mismatches",
            "gapopen: Gap openings",
            "qstart: Query alignment start",
            "qend: Query alignment end",
            "sstart: Subject alignment start",
            "send: Subject alignment end",
            "evalue: Expectation value",
            "bitscore: Bit score",
            sep = "\n"
          ))


          # refresh the error msg
          error_msg(NULL)
          error_sign(FALSE)

          # refresh the data save in the server: don't call directly
          # later::later(function() {
          #   lapply(c("data-raw/query.fa", "data-raw/blast_result"), function(f) file.create(f))
          # }, delay = 5)

      }%...!% {
        # On error, show message and clear previous outputs
        output$blast_msg <- renderText("BLAST failed. Please try again.")
        output$blast_table <- renderDT(NULL)
        output$display <- renderText("")

        later::later(function(){
          file.create("data-raw/query.fa")
        }, delay = 5)

      }

    }) # end of blast execution


    # show error msg if any
    observe({
      req(error_sign())
      output$blast_msg <- renderText(error_msg())
      output$blast_table <- renderDT(NULL)
    })

  })
 }


 # >seq1
 # CTAATAATAATGCAAACTATTTTAATTAGGTGTCAAACTTATTGGGGTAGTAGAATAGATTATAACATTAATTTAGGAGTTGTATTTATATTTTCTAAAACAACAATTAAGAGAATAAAGTAGAAAAGAAAAATAAAGAAAAGAAAGAAATAATAAGACAAAACAGTAAACAAAAAAAAAGATTGAAAGAGCCCCCCCCCCCGGGCCAAACGGCCCAGCTGGCCTCCGCCGCCCGAACGGGCCGGCCCAACCGGCCACCCCCCACTCCCCATAACCCCCCACTCCACCTGTCAAACCCTAGCCCACTCCCCACTTCCTCCTTCACGGTTCAGAATTTATATTGCGGGACGTTCGACCTCCAATTACATGTTTTAA

## To be copied in the UI
# mod_name_of_module1_ui("name_of_module1_1")

## To be copied in the server
# mod_name_of_module1_server("name_of_module1_1")
