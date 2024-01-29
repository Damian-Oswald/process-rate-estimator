# ---
# title: Sensitivity Analysis for the Process Rate Estimator
# author: Damian Oswald
# date: 2024-01-27
# ---

# PREPARE WORKSPACE
# =================

# add packages to search path
library(PRE)

# prepare PRE data
data <- calculateFluxes()

# define sample size
n <- 300

# sample parameters
parameters <- data.frame(
    #column = rep(c(1:5,7:12), each = 5, length = n),
    #depth = rep(getParameters()$depth, length = n),
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
                 column = 1, #p[["column"]],
                 depth = 7.5, #p[["depth"]],
                 n = 5,
                 parameters = do.call(getParameters, as.list(p)))
    return(x[["processes"]])
}

# apply `f` to all rows in `parameters`
results <- t(apply(parameters, 1, f))

# define list of expressions
expressions <- list(eta_SP_diffusion = expression(eta*"SP"[diffusion]),
                    eta_18O_diffusion = expression(eta^"18"*"O"[diffusion]),
                    SP_nitrification = expression("SP"[nitrification]),
                    d18O_nitrification = expression(delta^"18"*"O"[nitrification]),
                    SP_denitrification = expression("SP"[denitrification]),
                    d18O_denitrification = expression(delta^"18"*"O"[denitrification]),
                    eta_SP_reduction = expression(eta*"SP"[reduction]),
                    eta_18O_reduction = expression(eta^"18"*"O"[reduction]))

# compute standardized regression coefficients
SRC <- sapply(1:3,function(i) sensitivity::src(parameters, results[,i])[["SRC"]][,1])

# plot pairwise linear relationship
svg("sensitivity.svg", width = 12, height = 5)
par(mfrow = c(3,8), mar = rep(0,4)+0.2, oma = c(4,4,0,0), bg = "black", fg = "white")
for (j in 1:3) {
    for (i in 1:8) {
        x <- parameters[,i]
        y <- results[,j]
        plot(x = x, y = y, ylab = "", xlab = "", axes = FALSE, col = "transparent", xlim = c(min(parameters[,i]),max(parameters[,i])+diff(range(parameters[,i]))*0.1))
        grid(lty = 1)
        model <- lm(y ~ x)
        xr <- seq(min(x),max(x),l=100)
        yr <- predict(model, data.frame(x = xr), se.fit = TRUE)
        polygon(c(xr,rev(xr)),c(yr$fit+yr$se.fit*1.96,rev(yr$fit-yr$se.fit*1.96)), col = adjustcolor("red", alpha.f = 0.3), border = FALSE)
        lines(xr, yr$fit, lwd = 1.5, col = "red")
        points(x, y, cex = 0.5, pch = 16)
        w <- diff(par("usr"))[1]*0.04
        m <- mean(par("usr")[3:4])
        h <- 0.5*abs(SRC[i,j])*diff(par("usr")[3:4])
        
        polygon(x = c(max(xr)+2*w,max(xr)+w,max(xr)+w,max(xr)+2*w), y = c(m+h,m+h,m-h,m-h), col = par()$fg, border = FALSE)
        box()
        if(j==3) {
            title(xlab = expressions[[colnames(parameters)[i]]], xpd = NA)
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
Nitrification <- step(lm(results[,1] ~ ., data = parameters))
Denitrification <- step(lm(results[,2] ~ ., data = parameters))
Reduction <- step(lm(results[,3] ~ ., data = parameters))
sink("tbl-sensitivity.txt")
stargazer::stargazer(Nitrification, Denitrification, Reduction, type = "html", dep.var.labels = colnames(results))
sink()

Nitrification <- lme4::lmer(results[,1] ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction +  + (1 | column) * (1 | depth), data = parameters) |> summary()
Denitrification <- lme4::lmer(results[,2] ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction +  + (1 | column) * (1 | depth), data = parameters) |> summary()

Nitrification$coefficients[-1,1] |> dotchart()

# barplot of coefficients
df <- data.frame(Nitrification = coefficients(Nitrification),
                 Denitrification = coefficients(Denitrification),
                 Reduction = coefficients(Reduction))
barplot(t(df[-1,]), beside = TRUE)

sensitivity::src(X = parameters[,-c(1,2)], y = results[,2]) |> plot()
