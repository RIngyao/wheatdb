#' check_gene
#'
#' @description A utils function
#' @param gene string: gene ID
#' @param check string: what to check - size or  name or end
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
  }else if(check == "end"){
    # must end with numeric
    out <- stringr::str_detect(gene,"[0-9]$")
  }
  # else if(check == "num"){
  #   # must have numeric value after TraeCS1A
  #
  # }
  # }

  return(out)
}

