#' max_target
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
max_target <- function(input_val, default_val) {


  if(is.null(input_val) || is.na(input_val) || !is.numeric(input_val)) {
    value <- default_val
  }
  else {
    value <- input_val
  }

  return(value)
}
