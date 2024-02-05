# ---
# title: Sensitivity Analysis for the Process Rate Estimator
# author: Damian Oswald
# date: 2024-01-27
# ---

# PREPARE WORKSPACE
# =================

# add packages to search path
library(PRE)
library(sjPlot)
library(lme4)

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
SRC <- sapply(1:3, function(i) sensitivity::src(parameters, results[,i])[["SRC"]][,1])

# plot pairwise linear relationship
svg("sensitivity.svg", width = 12, height = 5)
par(mfrow = c(3,8), mar = rep(0,4)+0.2, oma = c(4,4,0,0))
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
        points(x, y, cex = 0.3, pch = 16)
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

coefficients <- lm(results ~ ., parameters)

for (i in 0:5) {
    model <- lm(results[1:50+i*50,1] ~ ., data = parameters[1:50+i*50,])
    print(summary(model))
}

df <- sapply(1:5, function(i) coef(lm(results[1:50+i*50,1] ~ ., data = parameters[1:50+i*50,])))

subset$group <- as.factor(paste("C",subset$column,"D",subset$depth,sep=""))

m1 <- lmer(Nitrification ~ 0 + eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (1 | depth) * (1 | column), data = data.frame(Nitrification = results[,1], parameters, subset))
m2 <- lmer(Denitrification ~ 0 + eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (1 | depth) * (1 | column), data = data.frame(Denitrification = results[,2], parameters, subset))
m3 <- lmer(Reduction ~ 0 + eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (1 | depth) * (1 | column), data = data.frame(Reduction = results[,3], parameters, subset))

tab_model(m1, m2, m3, show.ci = FALSE)

#Nitrification <- lme4::lmer(results[,1] ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction +  + (1 | column) * (1 | depth), data = parameters) |> summary()
#Denitrification <- lme4::lmer(results[,2] ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction +  + (1 | column) * (1 | depth), data = parameters) |> summary()

#Nitrification$coefficients[-1,1] |> dotchart()