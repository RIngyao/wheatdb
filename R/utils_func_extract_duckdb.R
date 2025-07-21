#' func_extract_duckdb
#'
#' @description A utils function to extract table from duckdb
#'
#' @return dplyr data, not data frame.
#' @param var character specifying the variant types - All, SNPs, InDels
#' @param group  character specifying the group name - landrace, pre, post, green, sphaero, wild, durum
#' @param db duckdb driver
#' @noRd

func_extract_duckdb <- function(var = "All", group = "All", db = duckdb::duckdb()){
  # browser()
  con <- dbConnect(db, "data-raw/final.duckdb")
  if(var == "All"){
    # include both SNPs and InDels
    if(length(group) > 1 || group == "All"){

      # read the group info data
      group_info <- read_delim("data-raw/groups.txt", delim = "\t")
      # constant columns
      cons_col <- c("CHROM", "POS","REF", "ALT", "TYPE", "IMPACT", "GENE") # column to always select

      if(length(group) > 1){

        # get the group accessions
        acc <- group_info[group_info$group == group, ]$name
        # extract only the accessions belong to the group
        snps_tbl <- tbl(con, "final_snps_total") %>% select(cons_col, acc) %>% mutate(variant="SNP")
        indels_tbl <- tbl(con, "final_indels_total") %>% select(cons_col, acc) %>% mutate(variant="InDel")

      } else {
        # user has selected all groups
        snps_tbl <- tbl(con, "final_snps_total") %>% mutate(variant="SNP") %>% mutate(variant="SNP")
        indels_tbl <- tbl(con, "final_indels_total") %>% mutate(variant="InDel") %>% mutate(variant="InDel")

      }

    } # end of group == All | > 1
    else {
      # get the SNPs
      snps_tbl <- dplyr::tbl(con,
                       switch(group,
                              "ILR" = "final_snps_ilr",
                              "IPR" = "final_snps_ipr",
                              "IPoR" = "final_snps_ipor",
                              "GR" = "final_snps_green",
                              "Sphaerococcum" = "final_snps_sphaero",
                              "Durum" = "final_snps_durum",
                              "Wild" = "final_snps_wild"
                       )
                       ) %>% mutate(variant="SNP")
      # get the InDels
      indels_tbl <- dplyr::tbl(con,
                            switch(group,
                                   "ILR" = "final_indels_ilr",
                                   "IPR" = "final_indels_ipr",
                                   "IPoR" = "final_indels_ipor",
                                   "GR" = "final_indels_green",
                                   "Sphaerococcum" = "final_indels_sphaero",
                                   "Durum" = "final_indels_durum",
                                   "Wild" = "final_indels_wild"
                            )
                            ) %>% mutate(variant="InDel")

    } # end group

    # Combine them (lazy) and arrange by chrom and position
    final <- dplyr::union_all(snps_tbl, indels_tbl) %>%
      arrange(CHROM, POS)
  } # end of var == All
  else{

    if (var == "SNPs"){
      if (length(group) > 1 || group == "All"){
        final <- dplyr::tbl(con, "final_snps_total") %>% mutate(variant="SNP")
      } # end group == All
      else{
        # get the SNPs
        final <- dplyr::tbl(con,
                          switch(group,
                                 "ILR" = "final_snps_ilr",
                                 "IPR" = "final_snps_ipr",
                                 "IPoR" = "final_snps_ipor",
                                 "GR" = "final_snps_green",
                                 "Sphaerococcum" = "final_snps_sphaero",
                                 "Durum" = "final_snps_durum",
                                 "Wild" = "final_snps_wild"
                          )
        ) %>% mutate(variant="SNP")
      } # end of group
    } # end of var = SNPs
    else if (var == "InDels"){

      if (length(group) > 1 || group == "All"){
        final <- dplyr::tbl(con, "final_indels_total") %>% mutate(variant="InDel")
      } # end group == All
      else{
        # get the indels
        final <- dplyr::tbl(con,
                         switch(group,
                                "ILR" = "final_indels_ilr",
                                "IPR" = "final_indels_ipr",
                                "IPoR" = "final_indels_ipor",
                                "GR" = "final_indels_green",
                                "Sphaerococcum" = "final_indels_sphaero",
                                "Durum" = "final_indels_durum",
                                "Wild" = "final_indels_wild"
                         )
        ) %>% mutate(variant="InDel")
      } # end of group
    } # end of var == "InDels"
  } # end of var

  return(final)
}


