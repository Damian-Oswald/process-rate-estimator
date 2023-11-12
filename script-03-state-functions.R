#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-02
#' ---

#' -----------------------------------------------------------------------------------------------------------
#' Prepare work space
#' -----------------------------------------------------------------------------------------------------------

library(np)

source("script-01-data-preparation.R")
remove(denominator, F_calc_bottom, F_calc_top, i, j, mask, value, concentration)

#' -----------------------------------------------------------------------------------------------------------
#' Derivative
#' -----------------------------------------------------------------------------------------------------------

#' Load the hyperparameters
load("resources/hyperparameters.RData")

#' Function to get the derivation of a kernel regression function
df <- data[data$column==1 & data$depth==30,]
visdat::vis_miss(df)

# numerical differentiation example
f <- function(t) t^2
t <- seq(-5,5,0.1)
pracma::fderiv(f, t)

#' -----------------------------------------------------------------------------------------------------------
#' Run solver
#' -----------------------------------------------------------------------------------------------------------

library(BB)

attach(data[complete[1],])

dN2O_dt <- 1 # get this from the approximated function
dSP_dt <- 1 # get this from the approximated function
d18O_dt <- 1 # get this from the approximated function

eqset <- function(x = c(N2Onit = NA, N2Oden = NA, N2Ored = NA), e = unlist(getEpsilons())) {
   f <- numeric(length(x))
   f[1] <- F_top_in + F_bottom_in - F_out + x[1] + x[2] - x[3] - dN2O_dt
   f[2] <- (F_top_in*(SP_top - e[3] - SP) + F_bottom_in*(SP_bottom - e[3] - SP) + x[1]*(e[1]-SP) + 
               x[2]*(e[2]-SP)-(e[3]*F_out + e[4]*x[3]))/N2O - dSP_dt
   f[3] <- (F_top_in*(d18O_top - e[7] - d18O) + F_bottom_in*(d18O_bottom - e[7] - d18O) + 
               x[1]*(e[5]-d18O) + x[2]*(e[6]-d18O)-(e[7]*F_out + e[8]*x[3]))/N2O - d18O_dt
   return(f)
}

par <- multiStart(par = matrix(runif(1000*3, 0, 40), ncol = 3), fn = eqset, action = "solve", method = 2, 
                  control = list(tol = 5, noimp = 100, maxit = 1500),
                  details = FALSE,
                  quiet = TRUE,
                  e = unlist(getEpsilons()))

barplot(apply(par$par, 2, mean))


