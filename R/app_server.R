
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # data---------------------------------
  # snps data
    snps_df = vroom::vroom("data-raw/snps_final_data.tsv", delim = "\t")


    blast_df = vroom::vroom("data-raw/snps_data.tsv", delim = "\t")

  # Your application server logic
  snp_table_server("table", snps_df=snps_df)

  # blast module
  blast_server("blast") #, blast_df=blast_df)

  # genetic resource module
  genetic_resources_server("resource")
}
