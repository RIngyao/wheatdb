#' e_value
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @param input_val numeric: provides user input
#' @param default_val numeric: shows default values if user input not given
#'
#' @noRd
e_value <- function(input_val, default_val) {
  if(is.null(input_val) || is.na(input_val) || !is.numeric(input_val)) {
    value <- default_val
  } else {
    value <- as.numeric(gsub(" ", "", trims(input_val)))
  }

  return(value)
}
