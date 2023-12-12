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

#' Load the prepared hyperparameters
load("resources/hyperparameters.Rdata")

#' Interpolate the missing values based on the bandwidths in `hyperparameters`
data <- getMissing(hyperparameters = hyperparameters)
# 1. this function interpolates all values over time
# 2. it also computes and adds the derivatives

#' Calculate fluxes from measurement data
data <- calculateFluxes(data = data, parameters = getParameters())
# 1. This function calculates all necessary parameters from the data.

#' Look at the derivatives
instpect <- function(xname = "N2O", Column = 1, Depth = 7.5) {
    df <- data[data$column==Column&data$depth==Depth,c("date",xname)]
    df2 <- PRE::measurements[data$column==Column&data$depth==Depth,c("date",xname)]
    plot(df, type = "l", ylim = c(min(0,min(na.omit(df2[,xname]))),max(na.omit(df2[,xname]))))
    points(df2)
    title(main = paste0("Column = ",Column," and depth = ",Depth))
}
instpect("N2O", Column = 3, Depth = 7.5)

data[data$column==1&data$depth==30,c("date","moisture")] |> plot(type = "l")

#' Visualize some results
boxplot(N2O ~ depth, data, outline = FALSE, log = "y")
boxplot(F_bottom_in ~ depth, data, outline = FALSE, log = "")
boxplot(SP ~ depth, data, outline = FALSE, log = "")


#' Compare old and new data
comparison = read.csv("/Users/answaltan/Desktop/new_data_C1_7.csv")
rangeComparison <- function(newname, oldname = newname) {
    x <- data[data$column==1 & data$depth==7.5, newname]
    y <- comparison[,oldname]
    boxplot(list(New = x, Old = y), outline = FALSE, ylab = newname, log = "y")
    if(t.test(x,y)$p.value<0.05) title(main = "Group differences detected") else title(main = "No group differences detected")
}
rangeComparison("N2ONarea", "N2O") # --> This is already not good...
rangeComparison("F_top_in")
rangeComparison("F_bottom_in")
rangeComparison("F_bottom_in")


#' -----------------------------------------------------------------------------------------------------------
#' Run solver
#' -----------------------------------------------------------------------------------------------------------

column <- 1
depth <- 7.5

#' try to run the state equations once with arbitrary starting values
stateEquations(
   x = c(N2Onit = 20, N2Oden = 20, N2Ored = 20),
   e = unlist(getEpsilons()),
   fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",])
)

#' run repeatedly with varying starting values using the multistart package
solution <- multiStart(par = matrix(runif(1000*3, 0, 40), ncol = 3),
                  fn = stateEquations,
                  action = "solve",
                  control = list(tol = 1e3),
                  details = FALSE,
                  quiet = TRUE,
                  e = unlist(getEpsilons()),
                  fluxes = as.list(data[data$column==column & data$depth==depth & data$date=="2015-09-02",]))

# selecting only best 2.5% of solutions
P <- with(solution, par)
P <- with(solution, par[fvalue < quantile(fvalue, probs = 0.025),])
P <- with(solution, par[converged,])
colnames(P) = c("N2Onit", "N2Oden", "N2Ored")

# plot results
cbind(parameter = rep(1:3, each = nrow(P)), value = as.numeric(P)) |> plot(cex = 0.8, col = "grey", pch = 16, axes = FALSE, xlab = "", ylab = "", xlim = c(0.5,3.5))
grid()
arrows(x0 = 1:3, y0 = apply(P, 2, quantile, probs = 0.025), y1 = apply(P, 2, quantile, probs = 0.975), code = 3, angle = 90)
arrows(x0 = 1:3, y0 = apply(P, 2, quantile, probs = 0.1), y1 = apply(P, 2, quantile, probs = 0.9), code = 3, angle = 90, lwd = 2)
axis(2, col = "transparent", col.ticks = par()$fg, las = 1)
points(1:3, colMeans(P), pch = 16)
axis(1, at = 1:3, labels = colnames(P), col = "transparent")

