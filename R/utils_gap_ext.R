#' gap_ext
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @param input_val numeric : user provided input
#' @param default_val numeric: default value if user does not provide a input
#'
#' @noRd

gap_ext <- function(input_val, default_val) {

  if(is.null(input_val) || is.na(input_val) || !is.numeric(input_val)) {
    value <- default_val
  }
  else {
    value <- as.numeric(gsub(" ", "", trims(input_val)))
  }

  return(value)
}
