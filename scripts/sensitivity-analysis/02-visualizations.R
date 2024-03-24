# ---
# title: Sensitivity Analysis for the Process Rate Estimator
# author: Damian Oswald
# date: 2024-01-27
# ---

# PREPARE WORKSPACE
# =================

# define colors
red <- "#fc5d5e"
blue <- "#3D5A80"
palette <- colorRampPalette(c(red,blue))
grey <- rgb(colorRamp(c("#3D5A80","white"))(0.5)/256)
palette2 <- colorRampPalette(c(grey,red))

# add packages to search path
library(PRE)
library(sjPlot)
library(lme4)

# read data
data <- read.csv(file.path("scripts","sensitivity-analysis","output","results-sensitivity-analysis.csv"))
for (i in 1:2) {
    data[,i] <- as.factor(data[,i])
}
data <- na.omit(data)

# save processes and parameters as variable
processes <- c("Nitrification", "Denitrification", "Reduction")
parameters <- c("BD", "eta_SP_diffusion", "eta_18O_diffusion", "SP_nitrification", "d18O_nitrification", "SP_denitrification", "d18O_denitrification", "eta_SP_reduction", "eta_18O_reduction")

# define list of expressions
expressions <- list(BD = expression("Bulk density [g cm"^-3*"]"),
                    eta_SP_diffusion = expression(eta*"SP"[diffusion]*" [‰]"),
                    eta_18O_diffusion = expression(eta^"18"*"O"[diffusion]*" [‰]"),
                    SP_nitrification = expression("SP"[nitrification]*" [‰]"),
                    d18O_nitrification = expression(delta^"18"*"O"[nitrification]*" [‰]"),
                    SP_denitrification = expression("SP"[denitrification]*" [‰]"),
                    d18O_denitrification = expression(delta^"18"*"O"[denitrification]*" [‰]"),
                    eta_SP_reduction = expression(eta*"SP"[reduction]*" [‰]"),
                    eta_18O_reduction = expression(eta^"18"*"O"[reduction]*" [‰]"))
expressions_unitless <- list(BD = expression("Bulk density"),
                             eta_SP_diffusion = expression(eta*"SP"[diffusion]),
                             eta_18O_diffusion = expression(eta^"18"*"O"[diffusion]),
                             SP_nitrification = expression("SP"[nitrification]),
                             d18O_nitrification = expression(delta^"18"*"O"[nitrification]),
                             SP_denitrification = expression("SP"[denitrification]),
                             d18O_denitrification = expression(delta^"18"*"O"[denitrification]),
                             eta_SP_reduction = expression(eta*"SP"[reduction]),
                             eta_18O_reduction = expression(eta^"18"*"O"[reduction]))
processlabels <- list(Nitrification = expression("Nitrification [g N"[2]*"O-N]"),
                      Denitrification = expression("Denitrification [g N"[2]*"O-N]"),
                      Reduction = expression("Reduction [g N"[2]*"O-N]"))


# FIGURE 6.1: PAIRWISE RESULTS OF THE PROCESS RATES
# =================================================

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
                    title(ylab = processlabels[[j]], xpd = NA, line = 2.5)
                    axis(2, las = 1)
                }
            }
        }
        dev.off()
    }
}

# FIGURE 7.1: COMBINATIONS OF SCATTERPLOTS OF THE ESTIMATED PROCESS RATES
# =======================================================================

f <- function(x, y, pos = NULL, ...) {
    plot(data[,c(x,y)], xlab = processlabels[[x]], ylab = processlabels[[y]], col = "transparent", ...)
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

# FIGURE 7.2: BARPLOT SHOWING THE DETERMINANT OF THE COVARIANCE MATRIX
# ====================================================================

# look at the magnitude of each covariance matrix
svg(file.path("scripts","sensitivity-analysis","output","covariance-norm.svg"), width = 8, height = 4)
par(mar = c(3,4.5,1,0)+0.2)
set.seed(0)
f <- function(c,d) {
    X <- as.matrix(subset(data, column==c & depth==d, c("Nitrification", "Denitrification")))
    det(cov(na.omit(X)))
}
df <- t(sapply(1:12, function(c) sapply(getParameters()$depths, function(d) f(c,d))))
b <- barplot(df, space = rep(c(1,rep(0.1,11)),5),
             beside = TRUE, log = "y",
             las = 1, col = rep(palette(5),each=12),
             ylim = c(20,20000), axes = FALSE,
             border = FALSE)
abline(h = c(10^(1:5), 2*10^(1:5), 5*10^(1:5)), lwd = 0.5)
axis(2, at = c(10^(1:5), 2*10^(1:5), 5*10^(1:5)), labels = c(10^(1:5), 2*10^(1:5), 5*10^(1:5)), las = 1)
box()
text(x = colMeans(b), y = 20, pos = 1, labels = paste(getParameters()$depth,"cm"), xpd = NA)
text(x = as.numeric(b), y = as.numeric(df), labels = 1:12, pos = 1, cex = 0.5, font = 2, col = "white")
title(ylab = expression("Determinant of the covariance matrix"), line = 3.5)
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
        
        # compute (adjusted) coefficient of determination
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


# FIGURE 6.2: RESULTS OF THE COEFFICIENTS FOR ALL PROCESSES AND PARAMETERS
# ========================================================================

svg(file.path("scripts","sensitivity-analysis","output","Coefficients.svg"), width = 12, height = 6)
par(mar = c(2,4,1,0)+0.1)
positions <- rep(5*(0:7), each = 3) + rep(1:3, 8)
centers <- 0.5*(positions[c(diff(positions)==3, FALSE)] + positions[c(FALSE, diff(positions)==3)])
df <- subset(results, Parameter!="BD")
df$Parameter <- factor(df$Parameter, levels = parameters[-1])
b <- boxplot(Coefficient ~ Process * Parameter, df, outline = FALSE, xlab = "", ylab = "Coefficients", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE)
abline(v = centers)
abline(h = seq(-2,3,0.25), lty = 3, lwd = 0.5)
abline(h = 0)
axis(2, las = 1)
box()
for (i in 1:3) {
    for (j in 1:8) {
        x <- subset(df, Process==processes[i]&Parameter==parameters[j+1])[,"Coefficient"]
        col <- palette2(256)
        points(x = rep(positions[j*3+i-3], length(x))+runif(length(x), -0.25, 0.25), y = x, cex = 0.6, pch = 16, col = col[ceiling(abs(x)*255/max(par("usr")[3:4]))])
    }
}
boxplot(Coefficient ~ Process * Parameter, outline = FALSE, df, xlab = "", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE, yaxs = "i", ylim = c(-1, 1), add = TRUE)
text(positions[7:9]+0.4, b$stats[1,7:9]-0.04, processes, srt = 90, pos = 2)
sapply(1:8, function(i) text(positions[(1:8)*3-1][i], par()$usr[3], expressions_unitless[[i+1]], xpd = NA, pos = 1))
dev.off()


# FIGURE 6.3: RESULTS OF THE STANDARDIZED REGRESSION COEFFICIENTS (SRC) FOR ALL PROCESSES AND PARAMETERS
# ======================================================================================================

svg(file.path("scripts","sensitivity-analysis","output","SRC.svg"), width = 12, height = 6)
par(mar = c(2,4,1,0)+0.1)
positions <- rep(5*(0:8), each = 3) + rep(1:3, 9)
centers <- 0.5*(positions[c(diff(positions)==3, FALSE)] + positions[c(FALSE, diff(positions)==3)])
b <- boxplot(SRC ~ Process * Parameter, outline = FALSE, results, xlab = "", ylab = "Standardized Regression Coefficients", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE, yaxs = "i", ylim = c(-1, 1))
abline(v = centers)
abline(h = seq(-1,1,0.1), lty = 3, lwd = 0.5)
abline(h = 0)
axis(2, las = 1)
box()
for (i in 1:3) {
    for (j in 1:9) {
        x <- subset(results, Process==processes[i]&Parameter==parameters[j])[,"SRC"]
        col <- palette2(256)
        points(x = rep(positions[j*3+i-3], length(x))+runif(length(x), -0.25, 0.25), y = x, cex = 0.6, pch = 16, col = col[ceiling(abs(x)*255)])
    }
}
boxplot(SRC ~ Process * Parameter, outline = FALSE, results, xlab = "", boxwex = 0.9, lty = 1, at = positions, col = "transparent", cex = 0.5, pch = 16, xaxs = "i", xlim = range(positions), axes = FALSE, yaxs = "i", ylim = c(-1, 1), add = TRUE)
text(positions[4:6]+0.4, b$stats[1,4:6]-0.02, processes, srt = 90, pos = 2)
sapply(1:9, function(i) text(positions[(1:9)*3-1][i], par()$usr[3], expressions_unitless[[i]], xpd = NA, pos = 1))
dev.off()


# VARIANCE DECOMPOSITION USING ANOVA
# ==================================

relativeSumOfSquares <- data.frame()
for (d in getParameters()$depth) {
  for (c in 1:12) {
    
    # make subset of current depth and column
    subset <- subset(data, subset = depth==d & column==c)
    
    # compute anova for each process
    for (process in processes) {
      
      # fit linear model
      model <- lm(subset[,process] ~ ., data = subset[,parameters])
      
      # save sum of squares
      SS <- anova(model)[["Sum Sq"]]
      
      # save results
      relativeSumOfSquares <- rbind(relativeSumOfSquares,
                                    data.frame(Depth = d,
                                               Column = c,
                                               Process = process,
                                               Source = rownames(anova(model)["Sum Sq"]), # save name of the source of sum of squares
                                               SS = SS,
                                               Relative_SS = SS/sum(SS))) # compute relative sum of squares
    }
  }
}

# compute the mean sum of squares over 12 * 5 anova
df <- sapply(processes, function(x) with(subset(relativeSumOfSquares, subset = Process==x),
                                         tapply(Relative_SS, Source, mean)))[rownames(anova(model)["Sum Sq"]),]

# compute R2 from results
R2 <- with(results, tapply(R2, Process, mean, na.rm = TRUE))

# check that the method of variance decomposition matches with the `lm` R2 method
print(apply(df[-10,], 2, sum) - R2)

# FIGURE 6.4: OVERALL RESULTS OF THE SENSITIVITY ANALYSIS
# =======================================================

# open svg file
svg(file.path("scripts","sensitivity-analysis","output","SRC-importances.svg"), width = 8, height = 8)

# set graphical parameters
par(mar = c(2,4,0,0)+0.1)

# alternative: use bar plot
plot.new()
plot.window(xlim = c(0,1.2), ylim = c(0,1))

# # compute share of explained variance with SRC
# df <- with(results, tapply(SRC, list(Parameter, Process), function(x) mean(abs(x), na.rm = TRUE)))
# 
# # standardize `df` such that each column adds up to the total explained variance
# for (i in 1:3) df[,i] <- df[,i]/(sum(df[,i])/R2[i])

# compute share of explained variance using the anova sum of squares
df <- sapply(processes, function(x) with(subset(relativeSumOfSquares, subset = Process==x),
                                         tapply(Relative_SS, Source, mean)))[rownames(anova(model)["Sum Sq"]),]

# define padding
padding <- 0.02

# define colors
colors <- palette(9)[c(1,8,5,6,4,7,3,9,2)]

# draw polygons
for (i in 1:3) {
    for (j in 1:9) {
        left <- c(0,cumsum(df[,i]))[j]
        right <- cumsum(df[,i])[j]
        polygon(y = c(left, right, right, left),
                x = c(i/3-padding,i/3-padding,i/3-1/3+padding,i/3-1/3+padding),
                col = if(df[j,i]>0.08) {
                    colors[j]
                } else colors[j])
        if(df[j,i]>0.08) {
            text(y = (left+right)/2-0.01, x = (i/3-padding + i/3-1/3+padding)/2, labels = expressions_unitless[[j]], pos = 3)
            text(y = (left+right)/2+0.01, x = (i/3-padding + i/3-1/3+padding)/2, labels = paste0(signif(df[j,i]*100,3),"%"), pos = 1)
        } else if(df[j,i]>0.04) {
            text(y = (left+right)/2, x = (i/3-padding + i/3-1/3+padding)/2, labels = paste0(signif(df[j,i]*100,3),"%"))
        }
    }
    polygon(y = c(R2[i], 1, 1, R2[i]),
            x = c(i/3-padding,i/3-padding,i/3-1/3+padding,i/3-1/3+padding),
            density = 25)
}

# label the axes
text(x = seq(0,1,l=7)[c(2,4,6)], y = c(-0.05), labels = processes, cex = 1.2, xpd = NA)
axis(2, at = seq(0,1,0.1), labels = paste0(seq(0,100,10)), las = 1)
title(ylab = "Variance explained [%]")

# add labels for yet missing parameters
f <- function(y0, y1 = y0, label) {
    lines(x = c(0.99, 1.05), y = c(y0, y1))
    text(1.05, y1, label = label, pos = 4)   
}
f(y0 = mean(c(1,R2[3])), label = expression("1 - R"^2))
f(y0 = mean(c(cumsum(df[,3])[2], c(0,cumsum(df[,3]))[2])),
  y1 = mean(c(cumsum(df[,3])[2], c(0,cumsum(df[,3]))[2]))-0.09,
  label = expressions_unitless[[2]])
f(y0 = mean(c(cumsum(df[,3])[3], c(0,cumsum(df[,3]))[3])),
  y1 = mean(c(cumsum(df[,3])[3], c(0,cumsum(df[,3]))[3]))-0.06,
  label = expressions_unitless[[3]])
f(y0 = mean(c(cumsum(df[,3])[4], c(0,cumsum(df[,3]))[4])),
  y1 = mean(c(cumsum(df[,3])[4], c(0,cumsum(df[,3]))[4]))-0.03,
  label = expressions_unitless[[4]])
f(y0 = mean(c(cumsum(df[,3])[5], c(0,cumsum(df[,3]))[5])),
  y1 = mean(c(cumsum(df[,3])[5], c(0,cumsum(df[,3]))[5])),
  label = expressions_unitless[[5]])
f(y0 = mean(c(cumsum(df[,3])[6], c(0,cumsum(df[,3]))[6])),
  y1 = mean(c(cumsum(df[,3])[6], c(0,cumsum(df[,3]))[6]))+0.03,
  label = expressions_unitless[[6]])
f(y0 = mean(c(cumsum(df[,3])[7], c(0,cumsum(df[,3]))[7])),
  y1 = mean(c(cumsum(df[,3])[7], c(0,cumsum(df[,3]))[7]))+0.06,
  label = expressions_unitless[[7]])
f(y0 = mean(c(cumsum(df[,3])[8], c(0,cumsum(df[,3]))[8])),
  y1 = mean(c(cumsum(df[,3])[8], c(0,cumsum(df[,3]))[8]))+0.09,
  label = expressions_unitless[[8]])

# close plotting
dev.off()
