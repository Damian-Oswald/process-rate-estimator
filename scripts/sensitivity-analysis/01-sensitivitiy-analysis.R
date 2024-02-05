# ---
# title: Sensitivity Analysis for the Process Rate Estimator
# author: Damian Oswald
# date: 2024-02-05
# ---

# EXPERIMENTS TO DETERMINE ADEQUATE SA PARAMETERS
# ===============================================

set.seed(42)

# SET PARAMETERS FOR SENSITIVITY ANALYSIS
# =======================================

COLUMNS = 1:12 # All the columns to compute 
DEPTHS = PRE::getParameters()$depths # A vector of depths to compute
SAMPLESIZE = 500 # How many samples should we draw from the parameter space?
SAMPLEREPEAT = 15 # What should be `n` in the `longPRE` function call?

# PREPARE WORKSPACE
# =================

# add packages to search path
library(PRE)

# prepare PRE data
data <- calculateFluxes()

# subset of the data
subset <- expand.grid(repetition = 1:SAMPLESIZE, column = COLUMNS, depth = DEPTHS)

# define sample size
n <- nrow(subset)

# sample parameters
parameters <- data.frame(
    eta_SP_diffusion = rnorm(n, 1.55, 0.28),
    eta_18O_diffusion = rnorm(n, -7.79, 0.27),
    SP_nitrification = runif(n, 26.2, 34.6),
    d18O_nitrification = rnorm(n, 36.5, 2),
    SP_denitrification = runif(n, -2.4, -0.9),
    d18O_denitrification = rnorm(n,11.1, 2),
    eta_SP_reduction = runif(n,-8,-2),
    eta_18O_reduction = runif(n,-24,-6)
    )

# visualize the sampled parameter space
png(file.path("scripts","sensitivity-analysis","output","pairs-panel.png"),
    width = 10,
    height = 10,
    unit = "in",
    res = 300)
pairs(parameters, pch = 16, cex = 0.5, col = adjustcolor(par("fg"), 0.1))
dev.off()

# COMPUTE SENSITIVITY ANALYSIS DATA
# =================================

# function to take one row of `parameters` and run PRE with that
f <- function(parameter, subset) {
    x <- longPRE(data,
                 column = subset[,"column"],
                 depth = subset[,"depth"],
                 n = SAMPLEREPEAT,
                 parameters = do.call(getParameters, as.list(parameter)),
                 verbose = FALSE)
    cat("\n", paste(subset[1,], collapse = ", "), "   ")
    return(x[["processes"]])
}

# apply `f` to all rows in `parameters`
results <- t(sapply(1:nrow(parameters), function(i) f(parameters[i,], subset[i,])))

# write the results as one CSV file
write.csv(x = data.frame(subset, parameters, results),
          file = file.path("scripts",
                           "sensitivity-analysis",
                           "output",
                           "results-sensitivity-analysis.csv"),
          row.names = FALSE)

