# ---
# title: Sensitivity Analysis for the Process Rate Estimator
# author: Damian Oswald
# date: 2024-01-27
# ---

# PREPARE WORKSPACE
# =================

red <- "#fc5d5e"
blue <- "#3D5A80"
palette <- colorRampPalette(c(red,blue))

# add packages to search path
library(PRE)
#library(sjPlot)
#library(lme4)

# read data
data <- read.csv(file.path("scripts","sensitivity-analysis","output","results-sensitivity-analysis.csv"))
for (i in 1:2) {
    data[,i] <- as.factor(data[,i])
}
data <- na.omit(data)

# save processes and parameters as variable
processes <- c("Nitrification","Denitrification","Reduction")
parameters <- c("BD", "eta_SP_diffusion", "eta_18O_diffusion", "SP_nitrification", "d18O_nitrification", "SP_denitrification", "d18O_denitrification", "eta_SP_reduction", "eta_18O_reduction")

# define list of expressions
expressions <- list(BD = "Bulk density",
                    eta_SP_diffusion = expression(eta*"SP"[diffusion]),
                    eta_18O_diffusion = expression(eta^"18"*"O"[diffusion]),
                    SP_nitrification = expression("SP"[nitrification]),
                    d18O_nitrification = expression(delta^"18"*"O"[nitrification]),
                    SP_denitrification = expression("SP"[denitrification]),
                    d18O_denitrification = expression(delta^"18"*"O"[denitrification]),
                    eta_SP_reduction = expression(eta*"SP"[reduction]),
                    eta_18O_reduction = expression(eta^"18"*"O"[reduction]))


# loop through all rows and columns
for (d in getParameters()$depth) {
    for (c in 1:12) {
        
        # make subset of current depth and column
        subset <- subset(data, subset = depth==d & column==c)
        
        # compute coefficients
        B <- coef(lm(cbind(Nitrification, Denitrification, Reduction) ~ ., subset[,c(parameters,processes)]))[-1,]
        
        # compute standardized regression coefficients
        SRC <- sapply(processes, function(i) sensitivity::src(subset[,parameters], subset[,i])[["SRC"]][,1])
        
        # plot pairwise linear relationship
        svg(file.path("scripts","sensitivity-analysis","output",sprintf("sensitivity-%s-%s.svg", c, d)), width = 13.5, height = 5)
        par(mfrow = c(3,9), mar = rep(0,4)+0.2, oma = c(4,4,0,0))
        for (j in 1:3) {
            for (i in 1:9) {
                x <- subset[,parameters][,i]
                y <- subset[,processes][,j]
                plot(x = x, y = y, ylab = "", xlab = "", axes = FALSE, col = "transparent", xlim = c(min(x),max(x)+diff(range(x))*0.1))
                grid(lty = 1)
                model <- lm(y ~ x)
                xr <- seq(min(x),max(x),l=100)
                yr <- predict(model, data.frame(x = xr), se.fit = TRUE)
                polygon(c(xr,rev(xr)),c(yr$fit+yr$se.fit*1.96,rev(yr$fit-yr$se.fit*1.96)), col = adjustcolor(red, alpha.f = 0.3), border = FALSE)
                lines(xr, yr$fit, lwd = 1.5, col = red)
                points(x, y, cex = 0.3, pch = 16)
                w <- diff(par("usr"))[1]*0.04
                m <- mean(par("usr")[3:4])
                h <- 0.5*abs(SRC[i,j])*diff(par("usr")[3:4])
                
                polygon(x = c(max(xr)+2*w,max(xr)+w,max(xr)+w,max(xr)+2*w), y = c(m+h,m+h,m-h,m-h), col = par()$fg, border = FALSE)
                box()
                if(j==3) {
                    title(xlab = expressions[[parameters[i]]], xpd = NA)
                    axis(1)
                }
                if(i==1) {
                    title(ylab = processes[j], xpd = NA)
                    axis(2, las = 1)
                }
            }
        }
        dev.off()
    }
}

# SCATTERPLOTS OF THE PROCESS RATES
# =================================

f <- function(x, y, pos = NULL, ...) {
    plot(data[,c(x,y)], col = "transparent", ...)
    colors <- palette(5)
    polygon(x = c(0,10000,10000,-10000,-10000,0), y = c(0,0,-10000,-10000,10000,10000), col = "gray90")
    grid(col = 1, lty = 1, lwd = 0.5)
    COLUMN <- 2
    for (c in 1:12) {
        for (d in PRE::getParameters()$depth) {
            X <- subset(data, column==c & depth==d, c(x, y))
            m <- mixtools::ellipse(mu = colMeans(X), sigma = cov(X), draw = FALSE)
            polygon(m[,1], m[,2], col = adjustcolor(colors[PRE::getParameters()$depth%in%d], 0.4), border = FALSE)
        }
    }
    box()
    points(data[,c(x,y)], pch = 16, cex = 0.2, col = colors[data$depth])
    if(!is.null(pos)) legend(pos, pch = 16, col = colors, pt.cex = 0.5, fill = adjustcolor(colors, 0.4), border = FALSE, legend = paste(PRE::getParameters()$depth, "cm"), title = "Depth", inset = c(0.02,0.02), bg = "white")
}

#svg(file.path("scripts","sensitivity-analysis","output","pairs-processes.svg"), width = 9, height = 6)
png(file.path("scripts","sensitivity-analysis","output","pairs-processes.png"), width = 9, height = 6, unit = "in", res = 450)
par(mfrow = c(2,3), mar = c(4,4,1,1)+0.1, oma = c(1,1,1,1))
f("Denitrification", "Nitrification", ylim = c(-7,42), xlim = c(0,55), las = 1)
f("Reduction", "Nitrification", ylim = c(-8,42), xlim = c(-30,50))
f("Denitrification", "Reduction", xlim = c(0,60), ylim = c(-10,50))
f("Denitrification", "Nitrification")
f("Reduction", "Nitrification")
f("Denitrification", "Reduction", "bottomright")
text(x = c(-365,-365), y = c(78, 276), labels = c("B", "A"), xpd = NA, cex = 3, font = 2)
dev.off()

 # look at the magnitude of each covariance matrix
svg(file.path("scripts","sensitivity-analysis","output","covariance-norm.svg"), width = 10, height = 5)
par(mar = c(3,4,1,0)+0.2)
set.seed(0)
f <- function(c,d) {
    X <- as.matrix(subset(data, column==c & depth==d, c("Nitrification", "Denitrification")))
    det(cov(X))
}
df <- t(sapply(1:12, function(c) sapply(getParameters()$depths, function(d) f(c,d))))
b <- barplot(df, space = rep(c(1,rep(0.1,11)),5),
             beside = TRUE, log = "y",
             las = 1, col = rep(palette(5),each=12),
             ylim = c(0.05,50000), axes = FALSE,
             border = FALSE)
abline(h = c(10^(-1:4)), lwd = 0.5)
axis(2, at = c(10^(-2:5)), labels = 10^(-2:5), las = 1)
box()
text(x = colMeans(b), y = 0.05, pos = 1, labels = paste(getParameters()$depth,"cm"), xpd = NA)
text(x = as.numeric(b), y = as.numeric(df), labels = 1:12, pos = 1, cex = 0.5, font = 2, col = "white")
title(ylab = expression("Determinant of the covariance matrix"))
title(xlab = "Depth", line = 2)
dev.off()

# visualize the sampled parameter space
png(file.path("scripts","sensitivity-analysis","output","pairs-panel.png"),
    width = 10,
    height = 10,
    unit = "in",
    res = 300)
pairs(data, pch = 16, cex = 0.5, col = adjustcolor(par("fg"), 0.1))
dev.off()


