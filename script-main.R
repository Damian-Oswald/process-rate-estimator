# ---
# title: Process Rate Estimator
# author: Damian Oswald
# date: 2023-10-02
# ---

# INSTALLING THE PACKAGE
# ======================

# load the `PRE` package from GitHub
remotes::install_github("https://github.com/Damian-Oswald/PRE")

# attach the package to the search path
library(PRE)

# LOADING THE DATA
# ================

# load the prepared hyperparameters
hyperparameters <- PRE::hyperparameters

# load the measurements used for the modelling
measurements <- PRE::measurements

# load the parameters for this session
parameters <- getParameters()

# CALCULATE MISSING DEPENDENT DATA
# ================================

# calculate N2O-N
original <- getN2ON(data = measurements, parameters = parameters)

# interpolate the missing values based on the bandwidths in `hyperparameters` (This function interpolates all values over time; and it also computes and adds the derivatives)
interpolated <- getMissing(data = original, hyperparameters = hyperparameters)

# calculate fluxes from measurement data (This function calculates all necessary parameters from the data)
data <- calculateFluxes(data = interpolated, parameters = parameters)

# RUN THE SOLVER ONCE
# ===================

# run the solver once
x <- PRE(data = data, column = 1, depth = 7.5, date = "2016-01-01", n = 200)

# print out information
print(x)

# plot
plot(x)

# pairs panel
pairs(x)

# RUN THE SOLVER OVER TIME
# ========================

# run the solver for all the dates
x <- longPRE(data, column = 1, depth = 7.5, n = 5)

# print information about the PRE results
print(x)

# plot result
plot(x)

# RUN THE SOLVER OVER THE ENTIRE DATASET
# ======================================

# Prepare an "overall" data frame
totalResults <- data.frame()

# Run for-loop on all
for (column in 1:2) {
    
    for (depth in getParameters()$depths) {

        # Write all results as PDF
        pdf(sprintf("results/PRE/Estimated-Process-Rates-C%s-D%s.pdf", column, depth), width = 8.27, height = 11.67)
        layout(mat = matrix(c(1,2,3,4,4,4,5,5,5,6,6,6), ncol = 3, byrow = TRUE))
        
        # run the solver for all the dates
        result <- longPRE(data, column = column, depth = depth, n = 10)
        
        # save dates
        dates <- data[data$column==column & data$depth==depth, "date"]
        
        # Visualize the (interpolated) model parameters
        par(mar = c(4,4,1,1)+0.5, oma = rep(2,4))
        for (x in c("N2ONarea", "SP", "d18O")) {
            ranges <- list(N2ONarea = c(0,10), SP = c(-8,23), d18O = c(22,55))
            plot(x = dates, y = data[data$column==column & data$depth==depth, x],
                 type = "l", lwd = 2, xlab = "Time", ylab = x, ylim = ranges[[x]])
            grid(col = 1)
            points(x = dates, y = original[data$column==column & data$depth==depth, x],
                   pch = 16, cex = 0.8)
        }
        plot(result)
        mtext(text = sprintf("Column %s at a depth of %s cm", column, depth), outer = TRUE, side = 3, adj = 0)
        dev.off()
        
        # print current results
        print(result)
        
        # bind current results to total results
        totalResults <- rbind(totalResults, result[["data"]])
        
        }
}

# Save the total results
write.csv(totalResults, "results/PRE/totalResults.csv")


# CREATE A SURROGATE MODEL
# ========================

model <- lm(`Reduction_50%` ~ N2ONarea + N2ONarea_derivative + SP + SP_derivative + d18O + d18O_derivative + F + theta_w, totalResults)
anova(model)
summary(model)
plot(predict(model), model$model$`Nitrification_50%`)



