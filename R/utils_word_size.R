#' word_size
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @param input_val integer: shows user input
#' @param default_val integer: shows default value  if user input not given
#'
#' @noRd
word_size <- function(input_val, default_val) {


  if(is.null(input_val) || is.na(input_val) || !is.numeric(input_val)) {
    value <- as.numeric(default_val)
  }
  else {
    value <- as.numeric(input_val)
  }

  return(value)
}
