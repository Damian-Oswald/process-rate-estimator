#' @title Get the optimal bandwidth parameters via hyperparameter tuning
#' 
#' @description
#' This function automatically optimizes the bandwidth for a specified combination of variables.
#' 
#' @param variables A character vector with variable names. Hyperparameters will be found for every combination of the specified variables.
#' @param data Data in a similar format as the [measurements] data.
#' 
#' @export
hypertune <- function(x = "N2O", data = PRE::measurements, variables = "column") {

}


damoswa::BayesOptim

Bayes