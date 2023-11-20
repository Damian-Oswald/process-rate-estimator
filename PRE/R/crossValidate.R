#' @title Perform `k`-fold cross-validation
#' 
#' @description A function to run an `r` times repeated `k`-fold cross validation of any sort of model.
#' 
#' @param FUN The model to repeatedly use as a function. Needs to take exactly four matrix arguments: `x_train`, `y_train`, `x_test`, and `y_test`. The function should return whatever evaluation metric we want to use.
#' @param x An \eqn{n * p} design matrix of the features to be used by the model.
#' @param y A vector of labels.
#' @param k Number of parts in which the data is split for the cross validation process.
#' @param r Number of times the cross validation process is repeated.
#' @param hyperparameter Hyperparameter for the model `FUN`
#' @param type Type of cross validation. Defaults to `"k-fold"`, though leave-one-out cross validation (`"LOOCV"`) is available as well.
#' 
#' @export
crossValidate <- function (FUN, x, y, k = nrow(x), r = 1, hyperparameter = NULL) {
   results <- data.frame()
   for (R in 1:r) {
      I <- matrix(c(sample(1:nrow(x)), rep(NA, k - nrow(x)%%k)), ncol = k, byrow = TRUE)
      for (K in 1:k) {
         i <- na.omit(I[,K])
         result <- cbind(cost = FUN(x_train = x[-i,], y_train = y[-i,], x_test = x[i,], y_test = y[i,], hyperparameter = hyperparameter), r = R, k = K)
         results <- rbind(results, result)
      }
   }
   return(results)
}