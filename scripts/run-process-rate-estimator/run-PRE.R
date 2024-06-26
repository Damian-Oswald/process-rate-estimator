# ---
# title: Process Rate Estimator
# author: Damian Oswald
# date: 2024-01-14
# description: This is the main script to run the process rate estimator (PRE) with some specified parameters
# ---

# PREPARATION
# ===========

# attach the package to the search path
library(PRE)

# SET PARAMETERS FOR THIS RUN
# ===========================

# number of samples of starting positions taken by the `BB::MultiStart` function
SAMPLENUMBER <- 1000

# columns to compute
COLUMNS <- 1:12

# depths to compute
DEPTHS <- getParameters()$depths

# LOADING THE DATA
# ================

# load the prepared hyperparameters
hyperparameters <- PRE::hyperparameters

# load the measurements used for the modelling
measurements <- PRE::measurements

# load the parameters for this session (these may be changed)
parameters <- getParameters()

# CALCULATE MISSING DEPENDENT DATA
# ================================

# calculate N2O-N
data <- getN2ON(data = measurements, parameters = parameters)

# interpolate the missing values based on the bandwidths in `hyperparameters` (This function interpolates all values over time; and it also computes and adds the derivatives)
data <- getMissing(data = data, hyperparameters = hyperparameters)

# calculate fluxes from measurement data (This function calculates all necessary parameters from the data)
data <- calculateFluxes(data = data, parameters = parameters)

# RUN THE SOLVER OVER THE ENTIRE DATASET
# ======================================

# Prepare an "overall" data frame
results <- data.frame()

# Run for-loop on all
for (column in COLUMNS) {
    
    for (depth in DEPTHS) {
        
        # run the solver for all the dates
        x <- longPRE(data, column = column, depth = depth, n = SAMPLENUMBER)
        cat("\n")
        
        # Write all results as an SVG file
        svg(file.path("scripts","run-process-rate-estimator","output",sprintf("visualized-process-rates-C%s-D%s.svg", column, depth)),
            width = 8*0.9, height = 10*0.9)
        
        # `try()` prevents crash when input data is missing
        try({
            par(mar = c(4,4,2,1)+0.1, oma = c(4,0,1,0)+0.1)
            layout(mat = matrix(c(1,2,3,4,5,6,4,5,6,4,5,6), nrow = 3, byrow = FALSE))
            plot(x, col = "#fc5d5e", layout = TRUE, ylim.processes = list(Nitrification = c(-10,60), Denitrification = c(-10,60), Reduction = c(-10,60)))
        })
        
        # close the writing process to SVG again
        dev.off()
        
        # print the results in the console
        print(x)
        
        # bind current results to total results
        results <- rbind(results, x[["data"]])
    }
}

# Save the total results
write.csv(results, file.path("scripts","run-process-rate-estimator","output","estimated-process-rates.csv"))
