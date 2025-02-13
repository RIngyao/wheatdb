#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # data---------------------------------
  # snps data
  df = vroom::vroom("data-raw/snps_data.tsv", delim = "\t")
  snps_df <- df[,1:30]
  # Your application server logic
  snp_table_server("table", snps_df=snps_df)
}
