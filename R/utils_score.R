#' score
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @param input_val numeric : value provided by user
#' @param check string: it should be either match or mismatch score
#'
#' @noRd
score <- function(input_val=NULL, check) {
  if(check == "reward") {
     if(is.null(input_val)) {
       value <- 2
     } else {
       value <- input_val
     }

  } else if (check == "penalty"){
    if(is.null(input_val)) {
      value <- -3
    } else {
      value <- input_val
    }
  }

  return(value)
}
