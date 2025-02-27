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

  #snps plot data
  df1 = vroom::vroom("data-raw/snps_effect_final.tsv")

  df_combined = cbind(df, df1)
  df_combined = df_combined[, !duplicated(colnames(df_combined))]

 # Your application server logic
  snp_table_server("table", snps_df=snps_df, snps_plot=df1, df_combined=df_combined)
}
