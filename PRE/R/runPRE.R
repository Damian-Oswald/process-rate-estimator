#' @title Run the Process Rate Estimator on one Date
#' 
#' @description
#' This function runs the Process Rate Estimator (PRE) on a specific depth, column and date.
#' 
#' @param data A data frame with all relevant variables to run the Process Rate Estimator.
#' @param column The column name for which we want to run the PRE.
#' @param depth The depth name for which we want to run the PRE.
#' @param date The date (`DD-MM-YYYY`) for which we want to run the PRE.
#' @param nonNegative Should negative solutions be removed before retrieving the quantiles?
#' @param n The number of samples solved with the [BB::multiStart] solver.
#' @param quantiles The probabilities of the quantiles to be returned from the sampled set.
#' @param verbose Should a success message be printed after a model run?
#' 
#' @export
runPRE <- function(data, column, depth, date, nonNegative = FALSE, n = 200, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), verbose = TRUE) {
    
    # run repeatedly with varying starting values using the multistart package
    solution <- multiStart(par = matrix(runif(n*3, 0, 40), ncol = 3),
                           fn = stateEquations,
                           action = "solve",
                           control = list(tol = 1e3),
                           details = FALSE,
                           quiet = TRUE,
                           e = unlist(getEpsilons()),
                           fluxes = as.list(data[data$column==column & data$depth==depth & data$date==date,]))
    
    # select all the converged solutions
    solution <- with(solution, par[converged,])
    colnames(solution) = c("N2Onit", "N2Oden", "N2Ored")
    
    # select only solutions for which all (estimated) processes are is non-negative
    if(nonNegative) solution <- solution[apply(solution, 1, function(x) all(x>0)),]
    
    # print out a success message
    if(verbose) cat(sprintf("\rPRE run with %s solutions (C%s D%s %s)", nrow(solution), column, depth, as.character(as.Date(date))))
    
    # return the quantiles on the solution
    apply(solution, 2, quantile, probs = quantiles)
}
