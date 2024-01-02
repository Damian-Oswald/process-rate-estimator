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

# Run for-loop on all
for (column in 8:12) {
    
    for (depth in getParameters()$depths) {
        
        # Run PRE on all combinations
        dates <- data[data$column==column & data$depth==depth, "date"]
        results <- lapply(dates, function(x) PRE::runPRE(data = data, column = column, depth = depth, date = x, nonNegative = FALSE))
        
        # Write all results as PDF
        cairo_pdf(sprintf("results/PRE/Estimated-Process-Rates-C%s-D%s.pdf", column, depth), width = 8.27, height = 11.67, onefile = TRUE)
        layout(mat = matrix(c(1,2,3,4,4,4,5,5,5,6,6,6), ncol = 3, byrow = TRUE))
        par(mar = c(4,4,1,1)+0.5, oma = rep(2,4))
        for (x in c("N2ONarea", "SP", "d18O")) {
            names <- c(N2ONarea = expression("N"[2]*"O-N"[area]),
                       SP = "SP",
                       d18O = expression("δ"^18*"O"))
            plot(x = dates, y = data[data$column==column & data$depth==depth, x],
                 type = "l", lwd = 2, xlab = "Time", ylab = names[x])
            grid(col = 1)
        }
        for (process in c("N2Onit", "N2Oden", "N2Ored")) {
            processnames <- c(N2Onit = expression("N"[2]*"O"[nitrification]),
                              N2Oden = expression("N"[2]*"O"[denitrification]),
                              N2Ored = expression("N"[2]*"O"[reduction]))
            plot(x = dates,
                 y = sapply(results, function(x) x[3,process]),
                 xaxs = "i", type = "l", ylab = processnames[process], xlab = "Time",
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
        mtext(text = sprintf("Column %s at a depth of %s cm", column, depth), outer = TRUE, side = 3, adj = 0)
        layout(matrix(1:9,nrow=3))
        par(mar = c(4,4,1,1)+0.5, oma = rep(2,4))
        for (x in c("N2ONarea", "SP", "d18O")) {
            for (process in c("N2Onit", "N2Oden", "N2Ored")) {
                plot(x = sapply(results, function(x) x[3,process]),
                     y = data[data$column==column & data$depth==depth, x],
                     xlab = processnames[process],
                     ylab = names[x], pch = 16)
                grid(col = 1)
            }
        }
        mtext(text = sprintf("Column %s at a depth of %s cm", column, depth), outer = TRUE, side = 3, adj = 0)
        dev.off()
        
        # Save file as JSON
        jsonlite::write_json(x = results, path = sprintf("results/PRE/PRE-results-C%s-D%s.json", column, depth), pretty = TRUE)
        
        cat(" ...Done!\n")
    }
}

# How to read it again...
jsonlite::read_json("results/PRE/PRE-results-C1-D7.5.json", simplifyVector = TRUE)



