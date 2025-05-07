#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # data---------------------------------
  # snps data
 # snps_df = vroom::vroom("data-raw/snps_final_data.tsv", delim = "\t")


 # blast_df = vroom::vroom("data-raw/snps_data.tsv", delim = "\t")

  # Your application server logic
 # snp_table_server("table")
  blast_server("blast")
}
