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

#' Load the parameters for this session
parameters <- getParameters()

#' Calculate N2O-N
PRE::measurements |>
    getN2ON(parameters = parameters) |>
    getMissing(hyperparameters = hyperparameters) |> # Interpolate the missing values based on the bandwidths in `hyperparameters` (This function interpolates all values over time; and it also computes and adds the derivatives)
    calculateFluxes(parameters = parameters) -> # Calculate fluxes from measurement data (This function calculates all necessary parameters from the data)
    data

#' Look at the derivatives
instpect <- function(xname = "N2O", Column = 1, Depth = 7.5) {
    df <- data[data$column==Column&data$depth==Depth,c("date",xname)]
    df2 <- PRE::measurements[data$column==Column&data$depth==Depth,c("date",xname)]
    plot(df, type = "l", ylim = c(min(0,min(na.omit(df2[,xname]))),max(na.omit(df2[,xname]))))
    points(df2)
    title(main = paste0("Column = ",Column," and depth = ",Depth))
}
instpect("N2O", Column = 2, Depth = 7.5)

data[data$column==1&data$depth==30,c("date","moisture")] |> plot(type = "l", ylim = c(0.3,0.35))

#' Visualize some results
boxplot(N2O ~ depth, data, outline = FALSE, log = "y")
boxplot(F_bottom_in ~ depth, data, outline = FALSE, log = "")
boxplot(SP ~ depth, data, outline = FALSE, log = "")


#' -----------------------------------------------------------------------------------------------------------
#' Run solver
#' -----------------------------------------------------------------------------------------------------------

runPRE <- function(data, column = 1, depth = 7.5, date = "2015-09-02") {
    
    # run repeatedly with varying starting values using the multistart package
    solution <- multiStart(par = matrix(runif(1000*3, 0, 40), ncol = 3),
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
    solution <- solution[apply(solution, 1, function(x) all(x>0)),]
    
    cat("\nPRE was succesfully run with ", nrow(solution), " solutions [column = ", column, ", depth = ", depth, ", date = ", date, "]", sep = "")
    
    # return the quantiles on the solution
    apply(solution, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
}

dates <- data[data$column==column & data$depth==depth, "date"]
results <- lapply(dates, function(x) runPRE(data = data, column = 1, depth = 7.5, date = x))


for (process in c("N2Onit", "N2Oden", "N2Ored")) {
    plot(x = dates,
         y = sapply(results, function(x) x[3,process]),
         xaxs = "i", type = "l", ylab = "Estimated process rate", xlab = "Time", main = process,
         ylim = range(sapply(results, function(x) x[1:5,process]), na.rm = TRUE))
    for (i in 1:2) {
        polygon(x = c(dates, rev(dates)),
                y = c(sapply(results, function(x) x[i,process]), rev(sapply(results, function(x) x[6-i,process]))),
                col = adjustcolor("cadetblue", alpha.f = 0.5*i),
                border = FALSE)
    }
    lines(x = dates, y = sapply(results, function(x) x[3,process]), lwd = 2)
    grid(col = 1)
}

# SP over time...
plot(x = dates, y = data[data$column==column & data$depth==depth, "SP"], type = "l")

# SP seems to massively drive the entire process for nitrification, but not so much for the other two...
plot(x = sapply(results, function(x) x[3,1]),
     y = data[data$column==column & data$depth==depth, "SP"])
