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
library(PRE)

#' Calculate fluxes from measurement data
data <- calculateFluxes(data = PRE::measurements, parameters = PRE::getParameters())

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

#' try to run the state equations once with arbitrary starting values
stateEquations(
   x = c(N2Onit = 1, N2Oden = 2, N2Ored = 3),
   e = unlist(getEpsilons()),
   derivatives = as.list(derivatives[derivatives$date=="2015-09-02",2:4]),
   fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",])
)

#' run repeatedly with varying starting values using the multistart package
ans <- multiStart(par = matrix(runif(50*3, 0, 40), ncol = 3),
                  fn = stateEquations,
                  action = "solve",
                  method = 2, 
                  control = list(tol = 5, noimp = 100, maxit = 1500),
                  details = FALSE,
                  quiet = TRUE,
                  e = unlist(getEpsilons()),
                  derivatives = as.list(derivatives[derivatives$date=="2015-09-02",2:4]),
                  fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",]))

# selecting only converged solutions
pmat <- ans$par[ans$converged,]
colnames(pmat) = c("N2Onit", "N2Oden", "N2Ored")
boxplot(pmat)
b <- barplot(apply(pmat, 2, mean), ylim = c(-10,30), col = "transparent") # how can we have negative values???
arrows(x0 = unlist(b), y0 = apply(pmat, 2, mean)+apply(pmat, 2, damoswa::se)*1.96, y1 = apply(pmat, 2, mean)-apply(pmat, 2, damoswa::se)*1.96, code = 3, angle = 90)

