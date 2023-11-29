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
data <- calculateFluxes(data = measurements, parameters = getParameters())

#' Visualize some results
boxplot(F ~ depth, data, outline = FALSE)

#' -----------------------------------------------------------------------------------------------------------
#' Derivative
#' -----------------------------------------------------------------------------------------------------------

#' Load the hyperparameters
load("resources/hyperparameters.RData")

#' Function to get the derivation of a kernel regression function
column <- 1
depth <- 30
variable <- "N2O"
bandwidth <- hyperparameters[which(depth%in%getParameters()$depths),column,variable]
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
   x = c(N2Onit = 20, N2Oden = 20, N2Ored = 20),
   e = unlist(getEpsilons()),
   derivatives = as.list(derivatives[derivatives$date=="2015-09-02",2:4]),
   fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",])
)

#' run repeatedly with varying starting values using the multistart package
solution <- multiStart(par = matrix(runif(500*3, 0, 40), ncol = 3),
                  fn = stateEquations,
                  action = "solve",
                  upper = c(40,40,40),
                  lower = c(0,0,0),
                  method = 2, 
                  control = list(tol = 5, noimp = 100, maxit = 1500),
                  details = FALSE,
                  quiet = TRUE,
                  e = unlist(getEpsilons()),
                  derivatives = as.list(derivatives[derivatives$date=="2015-09-02",2:4]),
                  fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",]))

# selecting only best 2.5% of solutions
P <- with(solution, par[fvalue < quantile(fvalue, probs = 0.025),])
P <- with(solution, par[converged & fvalue < quantile(fvalue, probs = 0.025),])
colnames(P) = c("N2Onit", "N2Oden", "N2Ored")

# plot results
cbind(parameter = rep(1:3, each = nrow(P)), value = as.numeric(P)) |> plot(cex = 0.8, col = "grey", pch = 16, axes = FALSE, xlab = "", ylab = "", xlim = c(0.5,3.5))
grid()
arrows(x0 = 1:3, y0 = apply(P, 2, mean)+apply(P, 2, damoswa::se)*1.96, y1 = apply(P, 2, mean)-apply(P, 2, damoswa::se)*1.96, code = 3, angle = 90)
axis(2, col = "transparent", col.ticks = par()$fg, las = 1)
points(1:3, colMeans(P), pch = 16)
axis(1, at = 1:3, labels = colnames(P), col = "transparent")

