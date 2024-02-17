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

# GENERATE TABLE SUMMARIES FROM MULTIPLE LINEAR MODELS
# ====================================================

# loop through all rows and columns
results <- data.frame()
for (d in getParameters()$depth) {
    for (c in 1:12) {
        
        # make subset of current depth and column
        subset <- subset(data, subset = depth==d & column==c)
        
        # compute coefficients
        B <- coef(lm(cbind(Nitrification, Denitrification, Reduction) ~ ., subset[,c(parameters,processes)]))[-1,]
        
        # compute standardized regression coefficients
        SRC <- sapply(processes, function(i) sensitivity::src(subset[,parameters], subset[,i])[["SRC"]][,1])

        X <- data.frame(parameter = NA, depth = d, column = c, parameters, cbind(B,SRC)[,c(1,4,2,5,3,6)])
        X[,1] <- rownames(X)
        colnames(X) <- c("parameter", "depth", "column", "parameters", "Nitrification", "Nitrification_SRC", "Denitrification", "Denitrification_SRC", "Reduction", "Reduction_SRC")
        results <- rbind(results, X)
    }
}

results$parameter <- as.factor(results$parameter)
boxplot(Nitrification ~ parameter, results, col = "coral", outline = FALSE, pch = 16, cex = 0.5, lty = 1, las = 1, ylab = "Standardized regression coefficients", at = 4*(1:8), boxwex = 1)
boxplot(Denitrification ~ parameter, results, col = "blue", outline = FALSE, pch = 16, cex = 0.5, las = 1, lty = 1, add = TRUE, at = 4*(1:8)+1, boxwex = 1)
grid(col=1)

results$parameter <- as.factor(results$parameter)
boxplot(Nitrification_SRC ~ parameter, results, col = "coral", pch = 16, cex = 0.5, lty = 1, ylim = c(-1,1), yaxs = "i", las = 1, ylab = "Standardized regression coefficients")
grid(col=1)

mu = with(results, tapply(Nitrification_SRC, parameters, mean))
sigma = with(results, tapply(Nitrification_SRC, parameters, sd))
dotchart(mu, xlim = c(-1,1))
for (i in 1:8) {
    lines(x = c(mu[i]+sigma[i],mu[i]-sigma[i]), y = c(i,i))
    points(mu[i],i,pch=16)
}



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

paste(parameters, collapse = " + ")

model_nit = lme4::lmer(Nitrification ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction || column:depth), data = data)
model_den = lme4::lmer(Denitrification ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction || column:depth), data = data)
model_red = lme4::lmer(Reduction ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction + (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction || column:depth), data = data)

model <- tab_model(model_nit, model_den, model_red,
                   show.ci = FALSE, show.p = TRUE, p.style = "numeric_stars",
                   terms = parameters)


save(model, file = file.path("scripts","sensitivity-analysis","output","lmer.RData"))

sink(file = file.path("resources", "tbl-sensitivity.txt"))
cat(model$knitr)
sink()

df <- as.data.frame(VarCorr(model_nit))[-1,c(2,4)]
barplot(df[,2], names.arg = c(df[-9,1],"Residuals"), las = 2)
dotchart(df[,2], labels = c(df[-9,1],"Residuals"), log = "x")




model_nit = lme4::lmer(Nitrification ~ eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction +
                           (eta_SP_diffusion + eta_18O_diffusion + SP_nitrification + d18O_nitrification + SP_denitrification + d18O_denitrification + eta_SP_reduction + eta_18O_reduction | column:depth),
                       data = data,
                       REML = TRUE,
                       verbose = 1,
                       #subset = sample(1:nrow(data),1000),
                       control = lmerControl(
                           optimizer = "Nelder_Mead",
                           optCtrl = list(iprint = 1000, maxfun = 10000, xst = 0.1)
                       ))
tab_model(model_nit)

