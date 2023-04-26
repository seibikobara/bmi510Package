#' This stores functions for BMI510 final project
#' 
#' 20 functions are documented in this package
#' 
#' @details 
#' @param expr code to evaluate
#' @keywords internal
#' @examples
#' should_stop(stop("Hi"))
should_stop <- function(expr){
    res <- try(print("hello"))
}

