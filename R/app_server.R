
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # new desing page ui---------------
  # show or hide the page using shinyjs

  observeEvent(input$page, {
    # print(paste("Page selected:", input$page))  # Confirm it fires

    all_pages <- c("home", "accessions", "variants", "morphology", "genome-tracks", "blast", "publications")
    lapply(all_pages, shinyjs::hide)
    shinyjs::show(input$page)
  })



# not work---------------
  # observeEvent(input$page, {
  #   all_pages <- c("home", "accessions", "variants", "morphology", "gtracks", "blast", "publications")
  #   lapply(all_pages, function(p) shinyjs::hide(p))
  #   shinyjs::show(input$page)
  #
  #   # Force iframe reload ONLY when gtracks page is shown
  #   output$gtracks_iframe <- renderUI({
  #     req(input$page == "gtracks")
  #     tags$iframe(
  #       src = "https://223.31.159.7/jb_wheatdb/?config=config.json&assembly=wheat&loc=Chr1A:39670..41695&tracks=wheat-ReferenceSequenceTrack,gene-annotations,All",
  #       height = "800px", width = "100%", style = "border: none;"
  #     )
  #   })
    # if (input$page == "gtracks") {
    #   shinyjs::runjs("
    #   var iframe = document.querySelector('#gtracks iframe');
    #   if (iframe) {
    #     var src = iframe.src;
    #     iframe.src = '';  // unload first
    #     iframe.src = src; // reload
    #   }
    # ")
    # }
  # })


  # observeEvent(input$page, {
  #   all_pages <- c("home", "accessions", "variants", "morphology", "gtracks", "blast", "publications")
  #
  #   lapply(all_pages, function(p) {
  #     shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'hidden';", p))
  #   })
  #
  #   shinyjs::runjs(sprintf("document.getElementById('%s').style.visibility = 'visible';", input$page))
  # })
  #
  # observeEvent(input$page, {
  #   if (input$page == "gtracks") {
  #     shinyjs::runjs("
  #     var iframe = document.querySelector('#gtracks iframe');
  #     iframe.src = iframe.src;
  #   ")
  #   }
  # })



  # end of new design-------------

  # data---------------------------------
  # Your application server logic
  # query variants
  snp_table_server("table")

  # load the accession detail
  acc_data <- read_delim("data-raw/groups.txt", delim = "\t")
  mod_accessions_server("accessions", accession_data = acc_data)

  # Seeds and spike detail
  genetic_resources_server("resource")

  # blast module
  blast_server("blast") #, blast_df=blast_df)
}
