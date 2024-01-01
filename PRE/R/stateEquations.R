#' @title State equation set
#' 
#' @description The state equation set are the core of the process rate estimator (PRE).
#' 
#' @param x Vector with starting values of `N2Onit`, `N2Oden`, and `N2Ored`.
#' @param e Vector of the isotope end members.
#' @param derivatives Derivatives of `N2ONarea`, `SP` and `d18O` with respect to time.
#' @param fluxes Values of `N2ONarea`, `SP` and `d18O` as well as their fluxes from other increments.
#' 
#' @details The state equation set incorporates three distinct equations.
#' \deqn{\frac{\Delta[N_2O]}{\Delta t}}
#' 
#' @export
stateEquations <- function(x = c(N2Onit = NA, N2Oden = NA, N2Ored = NA),
                           e = unlist(getEpsilons()),
                           fluxes = list(F_top_in = NA, F_bottom_in = NA, F_out = NA, SP_top = NA, SP = NA, SP_bottom = NA, N2ONarea = NA, d18O_top = NA, d18O_bottom = NA, d18O = NA)) {
   with(fluxes, {
      f <- numeric(3)
      f[1] <- F_top_in + F_bottom_in - F_out + x[1] + x[2] - x[3] - N2ONarea_derivative
      f[2] <- (F_top_in*(SP_top - e[3] - SP) + F_bottom_in*(SP_bottom - e[3] - SP) + x[1]*(e[1]-SP) + 
                  x[2]*(e[2]-SP)-(e[3]*F_out + e[4]*x[3]))/N2ONarea - SP_derivative
      f[3] <- (F_top_in*(d18O_top - e[7] - d18O) + F_bottom_in*(d18O_bottom - e[7] - d18O) + 
                  x[1]*(e[5]-d18O) + x[2]*(e[6]-d18O)-(e[7]*F_out + e[8]*x[3]))/N2ONarea - d18O_derivative
      return(f)
   })
}