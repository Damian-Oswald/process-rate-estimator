#' @title Get the derivative of a kernel regression function
#' 
#' @description This function numerically approximates the derivative of a function \eqn{f} through the central difference method.
#' This method computes the average rate of change over a small interval around the point of interest. The function \eqn{f} is estimated via kernel regression given `x`, `y` and a hyperparameter `bandwidth`.
#' 
#' @param x Independent variable.
#' @param y Dependent variable.
#' @param bandwidth Hyperparameter for the kernel regression.
#' @param newdata Optional vector of data points for which we want to calculate the derivative. If `NULL`, the derivative will be calculated for `x`.
#' @param n Order of derivative, should only be between 1 and 8. For `n = 0`, function values will be returned.
#' @param h Step size. By default, the step size will be set automatically. See details.
#' 
#' @details
#' The central difference method as implemented in the `pracma` package approximates the derivative of a function \eqn{f} as:
#' 
#' \deqn{\frac{df}{dt} \approx \frac{f(t + h) - f(t - h)}{2h}}
#' 
#' Here, \eqn{h} is a small positive number known as the step size.
#' The notations \eqn{f(t + h)} and \eqn{f(t - h)} represent the function values at \eqn{t + h} and \eqn{t - h}, respectively.
#' The smaller \eqn{h} is, the more accurate the approximation should be.
#' However, if \eqn{h} is too small, then round-off errors from computer arithmetic can
#' become significant. So, an appropriate balance must be struck.
#' 
#' @export
getDerivative <- function(x, y, bandwidth, newdata = NULL, n = 1, h = NULL) {
   model <- np::npreg(y ~ x, data = data.frame(x = x, y = y), bws = bandwidth)
   f <- function(x) predict(model, newdata = data.frame(x = x))
   pracma::fderiv(f = f, x = {if(is.null(newdata)) x else newdata}, n = n, h = if(is.null(h)) 0 else h)
}


