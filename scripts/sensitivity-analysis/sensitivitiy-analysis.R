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

n <- 180
parameters <- data.frame(
    column = rep(1:12, each = 5, length = n),
    depth = rep(getParameters()$depth, length = n),
    eta_SP_diffusion = rnorm(n, 1.55, 0.28),
    eta_18O_diffusion = rnorm(n, -7.79, 0.27),
    SP_nitrification = runif(n, 26.2, 34.6),
    d18O_nitrification = rnorm(n, 36.5, 2),
    SP_denitrification = runif(n, -2.4, -0.9),
    d18O_denitrification = rnorm(n,11.1, 2),
    eta_SP_reduction = runif(n,-8,-2),
    eta_18O_reduction = runif(n,-24,-6)
    )

# function to take one row of `parameters` and run PRE with that
f <- function(p) {
    x <- longPRE(data,
                 column = p[["column"]],
                 depth = p[["depth"]],
                 n = 5,
                 parameters = do.call(getParameters, as.list(p[-c(1:2)])),
                 verbose = TRUE)
    return(x[["processes"]])
}

# apply `f` to all rows in `parameters`
results <- t(apply(parameters, 1, f))

# plot pairwise linear relationship
svg("sensitivity.svg", width = 12, height = 5)
par(mfrow = c(3,8), mar = rep(0,4)+0.2, oma = c(4,4,0,0))
for (j in 1:3) {
    for (i in 1:8) {
        x <- parameters[,i+2]
        y <- results[,j]
        plot(x = x, y = y, ylab = "", xlab = "", axes = FALSE, col = "transparent")
        grid(lty = 1)
        model <- lm(y ~ x)
        xr <- seq(min(x),max(x),l=100)
        yr <- predict(model, data.frame(x = xr), se.fit = TRUE)
        polygon(c(xr,rev(xr)),c(yr$fit+yr$se.fit*1.96,rev(yr$fit-yr$se.fit*1.96)), col = adjustcolor("red", alpha.f = 0.3), border = FALSE)
        lines(xr, yr$fit, lwd = 2, col = "red")
        points(x, y, cex = 0.5, pch = 16)
        box()
        if(j==3) {
            title(xlab = colnames(parameters)[i+2], xpd = NA)
            axis(1)
        }
        if(i==1) {
            title(ylab = colnames(results)[j], xpd = NA)
            axis(2, las = 1)
        }
    }
}
dev.off()

# table of coefficients
Nitrification <- lm(results[,1] ~ as.factor(parameters$column) + as.factor(parameters$depth) + ., data = parameters)
Denitrification <- lm(results[,2] ~ ., data = parameters)
Reduction <- lm(results[,3] ~ ., data = parameters)
sink("tbl-sensitivity.txt")
stargazer::stargazer(Nitrification, Denitrification, Reduction, type = "html", dep.var.labels = colnames(results), omit = "Constant")
sink()

# barplot of coefficients
df <- data.frame(Nitrification = coefficients(Nitrification),
                 Denitrification = coefficients(Denitrification),
                 Reduction = coefficients(Reduction))
barplot(t(df[-1,]), beside = TRUE)


i <- is.nan(results[,1])
sensitivity::src(X = parameters[-i,], y = results[-i,1])
