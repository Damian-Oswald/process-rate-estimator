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

# read the prepared data
data <- read.csv("/Users/answaltan/Library/CloudStorage/OneDrive-Persönlich/Documents/Work/Hustles/SAE/process-rate-estimator/scripts/run-process-rate-estimator/output/estimated-process-rates.csv", row.names = 1)
cn <- colnames(data)
i <- which(cn %in% c("Nitrification_50.","Denitrification_50.","Reduction_50."))
cn[i] <- c("Nitrification", "Denitrification", "Reduction")
colnames(data) <- cn

# change some variable types
for (i in c("depth", "column", "variety")) {
    data[,i] %<>% as.factor()
}

# save variables
variables <- c("date", "column", "depth", "increment", "variety", "moisture", "N2O", "SP", "d18O", "N2ONvolume", "N2ONarea", "N2O_derivative", "N2ONvolume_derivative", "N2ONarea_derivative", "SP_derivative", "d18O_derivative", "theta_w", "theta_a", "Ds", "dCdZ", "F", "F_top_in", "F_top_out", "F_bottom_in", "F_bottom_out", "F_out", "SP_bottom", "d18O_bottom", "SP_top", "d18O_top")


apply(data[,variables], 2, function(x) mean(is.na(x)))

data <- data[,c(variables,"Nitrification","Denitrification","Reduction")]

data <- na.omit(data)

# Nitrification
model <- lm(Nitrification ~ ., data[,c(variables,"Nitrification")])
anova(model)
summary(model)
plot(predict(model), model$model[["Nitrification"]])

SA <- src(y = as.numeric(data[,"Nitrification"]),
          X = data.matrix(data[,variables[c(1:9,11:12,14:15)]]),
          nboot = 100, conf = 0.95)
ggplot2::ggplot(SA)

# Denitrification
model <- lm(Denitrification_50. ~ ., data[,c(variables,"Denitrification_50.")])
anova(model)
summary(model)
plot(predict(model), model$model$Denitrification_50.)

# Reduction
model <- lm(Reduction_50. ~ ., data[,c(variables,"Reduction_50.")])
anova(model)
summary(model)
plot(predict(model), model$model$Reduction_50.)

# TODO: Adjust the coefficients by their magnitude, or do z-score normalization

