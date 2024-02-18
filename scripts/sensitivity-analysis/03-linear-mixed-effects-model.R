# ---
# title: Sensitivity Analysis for the Process Rate Estimator
# author: Damian Oswald
# date: 2024-01-27
# ---

# PREPARE WORKSPACE
# =================

blue <- "#0C6EFC"

# add packages to search path
library(PRE)
library(sjPlot)
library(lme4)

# read data
data <- read.csv(file.path("scripts","sensitivity-analysis","output","results-sensitivity-analysis.csv"))
for (i in 2:3) {
    data[,i] <- as.factor(data[,i])
}

# save processes and parameters as variable
processes <- c("Nitrification","Denitrification","Reduction")
parameters <- c("eta_SP_diffusion", "eta_18O_diffusion", "SP_nitrification", "d18O_nitrification", "SP_denitrification", "d18O_denitrification", "eta_SP_reduction", "eta_18O_reduction")

# define list of expressions
expressions <- list(eta_SP_diffusion = expression(eta*"SP"[diffusion]),
                    eta_18O_diffusion = expression(eta^"18"*"O"[diffusion]),
                    SP_nitrification = expression("SP"[nitrification]),
                    d18O_nitrification = expression(delta^"18"*"O"[nitrification]),
                    SP_denitrification = expression("SP"[denitrification]),
                    d18O_denitrification = expression(delta^"18"*"O"[denitrification]),
                    eta_SP_reduction = expression(eta*"SP"[reduction]),
                    eta_18O_reduction = expression(eta^"18"*"O"[reduction]))

# GENERATE TABLE SUMMARIES FROM MULTIPLE LINEAR MODELS
# ====================================================

# loop through all rows and columns
results <- data.frame()
for (d in getParameters()$depth) {
    for (c in 1:12) {
        
        # make subset of current depth and column
        subset <- subset(data, subset = depth==d & column==c)
        
        # fit a linear model
        model <- lm(cbind(Nitrification, Denitrification, Reduction) ~ ., subset[,c(parameters,processes)])
        
        # compute coefficients
        B <- coef(model)[-1,]
        
        # compute standardized regression coefficients
        SRC <- sapply(processes, function(i) sensitivity::src(subset[,parameters], subset[,i])[["SRC"]][,1])

        # bind together the results        
        X <- data.frame(Depth = d,
                        Column = c,
                        reshape2::melt(B, value.name = "Coefficient", varnames = c("Parameter", "Process")),
                        SRC = reshape2::melt(SRC)[,3])
        R2 <- sapply(X$Process, function(i) getElement(getElement(summary(model), paste("Response",i)), "r.squared"))
        adjR2 <- sapply(X$Process, function(i) getElement(getElement(summary(model), paste("Response",i)), "adj.r.squared"))
        X <- data.frame(X, R2 = R2, adjR2 = adjR2)
        results <- rbind(results, X)
        
    }
}

# assign factor classes
results$Process <- ordered(results$Process, levels = processes)
results$Parameter <- ordered(results$Parameter, levels = parameters)

# write table
write.csv(results, file.path("scripts","sensitivity-analysis","output","coefficients.csv"), row.names = FALSE)


# DRAW BOXPLOT
# ============

# Color palette
palette <- colorRampPalette(c("lightgrey","red"))

# Coefficients
svg(file.path("scripts","sensitivity-analysis","output","Coefficients.svg"), width = 12, height = 6)
par(mar = c(2,4,1,0)+0.1)
positions <- rep(5*(0:7), each = 3) + rep(1:3, 8)
centers <- 0.5*(positions[c(diff(positions)==3, FALSE)] + positions[c(FALSE, diff(positions)==3)])
b <- boxplot(Coefficient ~ Process * Parameter, outline = FALSE, results, xlab = "", ylab = "Standardized Regression Coefficients", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE)
abline(v = centers)
abline(h = seq(-2,3,0.25), lty = 3, lwd = 0.5)
abline(h = 0)
axis(2, las = 1)
box()
for (i in 1:3) {
    for (j in 1:8) {
        x <- subset(results, Process==processes[i]&Parameter==parameters[j])[,"Coefficient"]
        col <- palette(256)
        points(x = rep(positions[j*3+i-3], length(x))+runif(length(x), -0.25, 0.25), y = x, cex = 0.6, pch = 16, col = col[ceiling(abs(x)*255/2.5)])
    }
}
boxplot(Coefficient ~ Process * Parameter, outline = FALSE, results, xlab = "", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE, yaxs = "i", ylim = c(-1, 1), add = TRUE)
text(positions[1:3]+0.4, b$stats[1,1:3]-0.04, processes, srt = 90, pos = 2)
sapply(1:8, function(i) text(positions[(1:8)*3-1][i], par()$usr[3], expressions[[i]], xpd = NA, pos = 1))
dev.off()

# Standardized regression coefficients
svg(file.path("scripts","sensitivity-analysis","output","SRC.svg"), width = 12, height = 6)
par(mar = c(2,4,1,0)+0.1)
positions <- rep(5*(0:7), each = 3) + rep(1:3, 8)
centers <- 0.5*(positions[c(diff(positions)==3, FALSE)] + positions[c(FALSE, diff(positions)==3)])
b <- boxplot(SRC ~ Process * Parameter, outline = FALSE, results, xlab = "", ylab = "Standardized Regression Coefficients", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE, yaxs = "i", ylim = c(-1, 1))
abline(v = centers)
abline(h = seq(-1,1,0.1), lty = 3, lwd = 0.5)
abline(h = 0)
axis(2, las = 1)
box()
for (i in 1:3) {
    for (j in 1:8) {
        x <- subset(results, Process==processes[i]&Parameter==parameters[j])[,"SRC"]
        col <- palette(256)
        points(x = rep(positions[j*3+i-3], length(x))+runif(length(x), -0.25, 0.25), y = x, cex = 0.6, pch = 16, col = col[ceiling(abs(x)*255)])
    }
}
boxplot(SRC ~ Process * Parameter, outline = FALSE, results, xlab = "", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE, yaxs = "i", ylim = c(-1, 1), add = TRUE)
text(positions[1:3]+0.4, b$stats[1,1:3]-0.02, processes, srt = 90, pos = 2)
sapply(1:8, function(i) text(positions[(1:8)*3-1][i], par()$usr[3], expressions[[i]], xpd = NA, pos = 1))
dev.off()

# Pie charts of importances
svg(file.path("scripts","sensitivity-analysis","output","SRC-importances.svg"), width = 6, height = 2)
par(mfrow = c(1,3), mar = c(0,0,2,0))
for (i in 1:3) {
    df <- with(results, tapply(SRC, list(Parameter, Process), function(x) mean(abs(x))))
    df[,i] |> pie(col = palette(256)[round(df[,i]*256)], labels = paste0("(",1:8,")"))
    title(processes[i], line = 0)
}
dev.off()

# COMPILE TABLE
# =============

f <- function(x) paste(signif(mean(x),2),"±",signif(sd(x),2), sep = "")
df <- with(results, tapply(SRC, list(Parameter, Process), f))
df <- rbind(df, R2 = with(results, tapply(R2, Process, f)), adj.R2 = with(results, tapply(adjR2, Process, f)))
knitr::kable(df, align = "rrr")

# COMPUTE R-SQUARED
# =================

f <- function(c,d,p){
    subset <- subset(data, subset = depth==d & column==c)
    summary(lm(subset[,processes][,p] ~ ., data = subset[,parameters]))$adj.r.squared
}

sapply(1:12, function(c) sapply(getParameters()$depth, function(d) f(c,d,"Nitrification"))) |> density() |> plot(main = "R-squared", xlim = c(0.5,1))
sapply(1:12, function(c) sapply(getParameters()$depth, function(d) f(c,d,"Denitrification"))) |> density() |> lines(lty = 2)
sapply(1:12, function(c) sapply(getParameters()$depth, function(d) f(c,d,"Reduction"))) |> density() |> lines(lty = 3)


# FIT LINEAR MIXED EFFECTS MODEL
# ==============================

# paste(parameters, collapse = " + ")
# 
# model_nit = lme4::lmer(Nitrification ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction || column:depth), data = data)
# model_den = lme4::lmer(Denitrification ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction || column:depth), data = data)
# model_red = lme4::lmer(Reduction ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction || column:depth), data = data)
# 
# model <- tab_model(model_nit, model_den, model_red,
#                    show.ci = FALSE, show.p = TRUE, p.style = "numeric_stars",
#                    terms = parameters)
# 
# 
# save(model, file = file.path("scripts","sensitivity-analysis","output","lmer.RData"))
# 
# sink(file = file.path("resources", "tbl-sensitivity.txt"))
# cat(model$knitr)
# sink()
# 
# df <- as.data.frame(VarCorr(model_nit))[-1,c(2,4)]
# barplot(df[,2], names.arg = c(df[-9,1],"Residuals"), las = 2)
# dotchart(df[,2], labels = c(df[-9,1],"Residuals"), log = "x")
# 
# 
# 
# 
# model_nit = lme4::lmer(Nitrification ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction +
#                            (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction | column:depth),
#                        data = data,
#                        REML = TRUE,
#                        verbose = 1,
#                        #subset = sample(1:nrow(data),1000),
#                        control = lmerControl(
#                            optimizer = "Nelder_Mead",
#                            optCtrl = list(iprint = 1000, maxfun = 10000, xst = 0.1)
#                        ))
# tab_model(model_nit)

