#' check_gene
#'
#' @description A utils function
#' @param gene string: gene ID
#' @param check string: what to check - size or name
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
check_gene <- function(gene, check){
  #browser()
  # for(gene in 1:length(list)){
  if(check == "name"){
    # check for presence of Traes
    out <- stringr::str_detect(gene,"TraesCS")
  }else if (check == "size"){
    gene <- strsplit(gene, "[, ]+")

    out <- (nchar(gene) == 19)
  }
  # }

  return(out)
}


?is_empty
