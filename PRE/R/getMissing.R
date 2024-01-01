#' @title Interpolate missing measurement data using kernel regression
#' 
#' @description This function interpolates the missing values of all variable names in `hyperparameters`. Also, it returns the derivatives of the smoothing functions used.
#' 
#' @param data The original data we wish to interpolate. Needs to have the same variables as [measurements].
#' @param hyperparameters An array of hyperparameters (bandwidths) used in the kernel regression process.
#' 
#' @export
getMissing <- function(data = PRE::measurements, hyperparameters) {
    for (variable in dimnames(hyperparameters)[[3]]) {
        for (column in dimnames(hyperparameters)[[2]]) {
            for (depth in dimnames(hyperparameters)[[1]]) {
                indices <- which(data[,"column"]==column & data[,"depth"]==as.numeric(depth))
                data[indices,variable] <- PRE::getDerivative(x = as.numeric(data[indices,"date"]),
                                                             y = data[indices,variable],
                                                             bandwidth = hyperparameters[as.character(depth),as.character(column),variable],
                                                             n = 0)
                data[indices,paste0(variable,"_derivative")] <- PRE::getDerivative(x = as.numeric(data[indices,"date"]),
                                                                                   y = data[indices,variable],
                                                                                   bandwidth = hyperparameters[as.character(depth),as.character(column),variable],
                                                                                   n = 1)
            }
        }
    }
    return(data)
}
