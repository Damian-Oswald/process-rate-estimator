# ---
# title: Process Rate Estimator
# author: Damian Oswald
# date: 2023-10-02
# ---

# load the `PRE` package from GitHub
remotes::install_github("https://github.com/Damian-Oswald/PRE")

# attach the package to the search path
library(PRE)

# load the prepared hyperparameters
load("resources/hyperparameters.Rdata")

# load the measurements used for the modelling
measurements <- PRE::measurements

# load the parameters for this session
parameters <- getParameters()

# calculate N2O-N
original <- getN2ON(data = PRE::measurements, parameters = parameters)

# interpolate the missing values based on the bandwidths in `hyperparameters` (This function interpolates all values over time; and it also computes and adds the derivatives)
interpolated <- getMissing(data = original, hyperparameters = hyperparameters)

# calculate fluxes from measurement data (This function calculates all necessary parameters from the data)
data <- calculateFluxes(data = interpolated, parameters = parameters)

# run the solver once
result <- PRE(data = data, column = 1, depth = 7.5, date = "2016-01-01")

# print out information
print(result)

# plot
plot(result)

# run the solver for all the dates
result <- longPRE(data, column = 2, depth = 7.5, n = 50, quantiles = c(0.01,0.5,0.99))

# print information about the PRE results
print(result)

# plot result
plot(result, which = "Nitrification")

# calculate the average non-negative rate
N2Onit <- nonNegative(results[,"N2Onit_50%"])

# Run for-loop on all
for (column in 1) {
    
    for (depth in 7.5) {

        # Write all results as PDF
        cairo_pdf(sprintf("results/PRE/Estimated-Process-Rates-C%s-D%s.pdf", column, depth), width = 8.27, height = 11.67, onefile = TRUE)
        layout(mat = matrix(c(1,2,3,4,4,4,5,5,5,6,6,6), ncol = 3, byrow = TRUE))
        par(mar = c(4,4,1,1)+0.5, oma = rep(2,4))
        for (x in c("N2ONarea", "SP", "d18O")) {
            names <- c(N2ONarea = expression("N"[2]*"O-N"[area]),
                       SP = "SP",
                       d18O = expression("δ"^18*"O"))
            ranges <- list(N2ONarea = c(0,10),
                           SP = c(-8,23),
                           d18O = c(22,55))
            plot(x = dates, y = data[data$column==column & data$depth==depth, x],
                 type = "l", lwd = 2, xlab = "Time", ylab = names[x], ylim = ranges[[x]])
            grid(col = 1)
            points(x = dates, y = original[data$column==column & data$depth==depth, x],
                   pch = 16, cex = 0.8)
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
            abline(h = 0)
        }
        mtext(text = sprintf("Column %s at a depth of %s cm", column, depth), outer = TRUE, side = 3, adj = 0)
        dev.off()
        
        # Save file as JSON
        jsonlite::write_json(x = results, path = sprintf("results/PRE/PRE-results-C%s-D%s.json", column, depth), pretty = TRUE)
        
        cat(" ...Done!\n")
    }
}



