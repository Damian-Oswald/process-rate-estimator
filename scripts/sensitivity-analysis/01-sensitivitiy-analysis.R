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
SAMPLESIZE = 20 # How many samples should we draw from the parameter space?
SAMPLEREPEAT = 3 # What should be `n` in the `longPRE` function call?

# PREPARE WORKSPACE
# =================

# add packages to search path
library(PRE)

# define sample size
n <- SAMPLESIZE * length(COLUMNS) * length(DEPTHS)

# sample parameters
parameters <- data.frame(
    expand.grid(repetition = 1:SAMPLESIZE, column = COLUMNS, depth = DEPTHS)[sample(1:n),-1],
    BD = sort(rep(getParameters()$BD+seq(-0.1,0.1,l=11),l=n)),
    eta_SP_diffusion = rnorm(n, 1.55, 0.28),
    eta_18O_diffusion = rnorm(n, -7.79, 0.27),
    SP_nitrification = runif(n, 32, 38.7),
    d18O_nitrification = rnorm(n, 23.5, 3),
    SP_denitrification = runif(n, -13.6, 3.7),
    d18O_denitrification = rnorm(n, 11.1, 2),
    eta_SP_reduction = runif(n,-8,-2),
    eta_18O_reduction = runif(n,-18,-12)
    )

# COMPUTE SENSITIVITY ANALYSIS DATA
# =================================

# run f over all the sampled parameters
results <- data.frame(Nitrification = rep(NA, n), Denitrification = rep(NA, n), Reduction = rep(NA, n))
BD <- 0
for (i in 1:n) {
    
    # check if we have to calculate fluxes with a new bulk density
    if(!(parameters[i,"BD"]==BD)){
        BD <- parameters[i,"BD"]
        P <- getParameters(BD = BD)
        data <- PRE::measurements |>
            getN2ON(P) |>
            getMissing() |>
            calculateFluxes(P, FALSE)
    }
    
    # run model for the specific parameter set
    results[i,] <- longPRE(data = data,
                           column = parameters[i,"column"],
                           depth = parameters[i,"depth"],
                           n = SAMPLEREPEAT,
                           parameters = do.call(getParameters, as.list(parameters[i,-(1:3)])),
                           verbose = FALSE)[["processes"]]
    
    # print out progress
    PRE:::progressbar(i,n)
}

# write the results as one CSV file
write.csv(x = data.frame(parameters, results),
          file = file.path("scripts",
                           "sensitivity-analysis",
                           "output",
                           "results-sensitivity-analysis.csv"),
          row.names = FALSE)

