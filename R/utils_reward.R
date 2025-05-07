#' reward
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @param input_val numeric: shows user input
#' @param default_val numeric: shows default value  if user input not given
#'
#' @noRd

reward <- function(input_val, default_val) {

  if(is.null(input_val) || is.na(input_val) || !is.numeric(input_val)) {
    value <- default_val
  }
  else {
    value <- as.numeric(gsub(" ", "", trims(input_val)))
  }
  return(value)
}
