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

# load the parameters with an alternative parameter value
parameters <- getParameters(BD = 1.7)

# CALCULATE MISSING DEPENDENT DATA
# ================================

# calculate N2O-N
data <- getN2ON(data = measurements, parameters = parameters)

# interpolate the missing values based on the bandwidths in `hyperparameters` (This function interpolates all values over time; and it also computes and adds the derivatives)
data <- getMissing(data = data, hyperparameters = hyperparameters)

# calculate fluxes from measurement data (This function calculates all necessary parameters from the data)
data <- calculateFluxes(data = data, parameters = parameters)

# read some details on a function
help(calculateFluxes)

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
plot(x, which = "Nitrification")

# plot overview
plot(x)

# plot overview with fixed y-axis limits
plot(x, ylim.processes = list(Nitrification = c(-50,50), Denitrification = NA, Reduction = NA))


# CREATE A SURROGATE MODEL
# ========================

data <- read.csv("/Users/answaltan/Library/CloudStorage/OneDrive-Persönlich/Documents/Work/Hustles/SAE/process-rate-estimator/scripts/run-process-rate-estimator/output/estimated-process-rates.csv", row.names = 1)

model <- lm(Nitrification_50. ~ ., data[,c(colnames(data)[1:35],"Nitrification_50.")])
anova(model)
summary(model)
plot(predict(model), model$model$Nitrification_50.)
coefplot::coefplot(model)

model <- lm(Denitrification_50. ~ ., data[,c(colnames(data)[1:35],"Denitrification_50.")])
anova(model)
summary(model)
plot(predict(model), model$model$Denitrification_50.)
coefplot::coefplot(model)

model <- lm(Reduction_50. ~ ., data[,c(colnames(data)[1:35],"Reduction_50.")])
anova(model)
summary(model)
plot(predict(model), model$model$Reduction_50.)
coefplot::coefplot(model)

# TODO: Adjust the coefficients by their magnitude, or do z-score normalization

