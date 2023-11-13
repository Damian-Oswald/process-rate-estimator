#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-02
#' ---

#' -----------------------------------------------------------------------------------------------------------
#' Prepare work space
#' -----------------------------------------------------------------------------------------------------------

library(BB)
library(np)

source("script-01-data-preparation.R")
remove(denominator, F_calc_bottom, F_calc_top, i, j, mask, value, concentration)

#' -----------------------------------------------------------------------------------------------------------
#' Derivative
#' -----------------------------------------------------------------------------------------------------------

#' Load the hyperparameters
load("resources/hyperparameters.RData")

#' Function to get the derivation of a kernel regression function
column <- 1
depth <- 7.5
variable <- "N2O"
bandwidth <- hyperparameters[which(depth%in%depths),column,variable]
subset <- data[data$column == column & data$depth == depth,]
date <- subset[,"date"]
derivatives <- data.frame(date = subset[,"date"])
for(variable in c("N2O", "SP", "d18O")) derivatives[,paste0(variable,"_derivative")] <- getDerivative(x = as.numeric(date), subset[,variable], bandwidth)

#' -----------------------------------------------------------------------------------------------------------
#' Run solver
#' -----------------------------------------------------------------------------------------------------------

data[data$column==column & data$depth==depth & data$date=="2015-09-02",]
derivatives[derivatives$date=="2015-09-02",2:4]



stateEquations(
   x = c(N2Onit = 1, N2Oden = 2, N2Ored = 3),
   e = unlist(getEpsilons()),
   derivatives = as.list(derivatives[derivatives$date=="2015-09-02",2:4]),
   fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",])
)

par <- multiStart(par = matrix(runif(500*3, 0, 40), ncol = 3),
                  fn = stateEquations,
                  action = "solve",
                  method = 2, 
                  control = list(tol = 5, noimp = 100, maxit = 1500),
                  details = FALSE,
                  quiet = TRUE,
                  e = unlist(getEpsilons()),
                  derivatives = as.list(derivatives[derivatives$date=="2015-09-02",2:4]),
                  fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",]))

barplot(apply(par$par, 2, mean))


