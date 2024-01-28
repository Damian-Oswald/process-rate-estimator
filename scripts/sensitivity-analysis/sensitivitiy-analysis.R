# ---
# title: Sensitivity Analysis for the Process Rate Estimator
# author: Damian Oswald
# date: 2024-01-27
# ---

# PREPARE WORKSPAEC
# =================

# add packages to search path
library(PRE)

# prepare PRE data
data <- calculateFluxes()

n <- 200
parameters <- data.frame(eta_SP_diffusion = rnorm(n, 1.55, 0.28),
                         eta_18O_diffusion = rnorm(n, -7.79, 0.27),
                         SP_nitrification = runif(n, 26.2, 34.6),
                         d18O_nitrification = rnorm(n, 36.5, 2),
                         SP_denitrification = runif(n, -2.4, -0.9),
                         d18O_denitrification = rnorm(n,11.1, 2),
                         eta_SP_reduction = runif(n,-8,-2),
                         eta_18O_reduction = runif(n,-24,-6))
pairs(parameters)

# function to take one row of `parameters` and run PRE with that
f <- function(p) {
    x <- longPRE(data, 1, 7.5, n = 5, parameters = do.call(getParameters, as.list(p)))
    apply(x, 2, mean)
}

# apply `f` to all rows in `parameters`
results <- t(apply(parameters, 1, f))

s <- summary(lm(results ~ ., data = parameters))

svg("sensitivity.svg", width = 24, height = 9)
par(mfrow = c(3,8), mar = rep(2,4)+0.1, oma = c(4,4,0,0))
for (j in 1:3) {
    for (i in 1:8) {
        plot(x = parameters[,i], y = results[,j], ylab = "", xlab = "", pch = 16, col = "grey")
        grid(lty = 1)
        title(main = paste("p =",signif(s[[j]][["coefficients"]][1+i,4],2)), line = -1)
        abline(lm(results[,j] ~ parameters[,i]))
        if(j==3) title(xlab = colnames(parameters)[i], xpd = NA)
        if(i==1) title(ylab = colnames(results)[i], xpd = NA)
    }
}
dev.off()

Nitrification <- step(lm(results[,1] ~ ., data = parameters))
Denitrification <- step(lm(results[,2] ~ ., data = parameters))
Reduction <- step(lm(results[,3] ~ ., data = parameters))
sink("tbl-sensitivity.txt")
stargazer::stargazer(Nitrification, Denitrification, Reduction, type = "html", dep.var.labels = colnames(results))
sink()

