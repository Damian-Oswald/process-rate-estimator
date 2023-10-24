#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-02
#' ---

#' -----------------------------------------------------------------------------------------------------------
#' Attach packages to search path
#' -----------------------------------------------------------------------------------------------------------

library(magrittr)
library(np)
library(animation)

#' -----------------------------------------------------------------------------------------------------------
#' Define global variables
#' -----------------------------------------------------------------------------------------------------------

palette <- colorRampPalette(c("#66999B","#0E4749","#E55812","#EFE7DA"))

#' -----------------------------------------------------------------------------------------------------------
#' Read data and parameters
#' -----------------------------------------------------------------------------------------------------------

#' Read parameters
parameters <- jsonlite::read_json("resources/parameters.json", simplifyVector = TRUE)

#' Attach parameter list to global environment
attach(parameters)

#' Read the prepared data
data <- read.csv("data/data-calculated.csv")

#' Transform the variable `date_R` to `Date` class
data[,"date"] %<>% as.Date

#' Transform `column` to `ordered` to `integer`
data[,"column"] %<>% as.ordered

#' Transform `variety` from `character` to `factor`
data[,"variety"] %<>% as.factor()

#' Pre-define a data frame of every possible combination
variables <- c("gN2ONha", "SP", "d18O")
bandwidths <- exp(seq(log(5),log(100),l=50))
results <- expand.grid(variable = variables,
                       column = 1:12,
                       depth = depths,
                       bandwidth = bandwidths,
                       cost = NA)

#' Progressbar
progressbar <- function (i, n) {
   w <- (options("width")$width - 7)/n
   cat("\r[", strrep("=", ceiling(i * w)), ">", strrep("-", floor((n - i) * w)), "] ", paste0(format(round(i/n * 100, 1), nsmall = 1), "%"), sep = "")
}

#' -----------------------------------------------------------------------------------------------------------
#' Cross-validation of the curve smoothing
#' -----------------------------------------------------------------------------------------------------------

#' Function to `k`-fold cross-validate a model `FUN` for `r` times given the hyperparameter `theta`
cross_validate <- function (FUN, x, y, k = nrow(x), r = 1, hyperparameter = NULL) {
   results <- data.frame()
   for (R in 1:r) {
      I <- matrix(c(sample(1:nrow(x)), rep(NA, k - nrow(x)%%k)), ncol = k, byrow = TRUE)
      for (K in 1:k) {
         i <- na.omit(I[,K])
         result <- cbind(cost = FUN(x_train = x[-i,], y_train = y[-i,], x_test = x[i,], y_test = y[i,], hyperparameter = hyperparameter), r = R, k = K)
         results <- rbind(results, result)
      }
   }
   return(results)
}

#' Define model evaluation function `FUN`
options(np.messages = FALSE) # Turn off messages by the npreg function
FUN <- function(x_train, x_test, y_train, y_test, hyperparameter) {
   
   # Create two data frames `train` and `test`
   train <- data.frame(x = x_train, y = y_train)
   test <- data.frame(x = x_test, y = y_test)
   
   # Calibrate the model given the `train` data frame
   model <- np::npreg(y ~ x, data = train, bws = hyperparameter)
   
   # Evaluate the model on the `test` data frame
   y_hat <- predict(model, newdata = test)
   
   # Return the Cost function value, here the Root Mean Squared Error (RMSE)
   sqrt(mean((y_hat - y_test)^2))
}

#' Run all cross-validations
for (i in 1:nrow(results)) {
   
   # Cache a subset of the data for this cross-validation
   subset <- na.omit(data[data$column==results[i,"column"] & data$depth==results[i,"depth"], c("date",as.character(results[i,"variable"]))])
   
   # Run cross-validation
   cv <- cross_validate(FUN = FUN, 
                        x = as.matrix(as.numeric(subset[,"date"])),
                        y = as.matrix(subset[,as.character(results[i,"variable"])]),
                        hyperparameter = results[i,"bandwidth"],
                        k = 3,
                        r = 10)
   
   # Calculate mean cost of all repeated model calibrations
   results[i,"cost"] <- mean(cv[,"cost"], na.rm = TRUE)
   
   # Print the progress
   progressbar(i, nrow(results))
}

#' Save the results as a CSV
write.csv(results, "data/results-hypertuning.csv", row.names = FALSE)

#' Save the best hyperparameter for each combination of both depth and variable
with(results, tapply(cost, list(bandwidth, depth, column, variable), mean)) |>
   apply(2:4, function(cost) unique(results$bandwidth)[which.min(cost)]) ->
   hyperparameters
df <- as.data.frame.table(hyperparameters, responseName = c("bandwidth"))
colnames(df) <- c("depth","column","variable","bandwidth")
write.csv(df, file = "resources/hyperparameters.csv")
save(hyperparameters, file = "resources/hyperparameters.RData")

#' Visualize pick of the hyperparameter
pdf("results/hypertuning.pdf", width = 5*4, height = 3*4)
par(mfrow = c(length(variables),length(depths)))
for (variable in variables) {
   for (depth in depths) {
      subset <- results[results$variable==variable & results$depth == depth,]
      mean <- with(subset, tapply(cost, bandwidth, mean))
      se <- with(subset, tapply(cost, bandwidth, function(x) return(sd(x)/sqrt(length(na.omit(x))))))
      plot(mean ~ bandwidths, log = "xy", ylim = range(c(mean + se, mean - se)),
           xlab = "Bandwidth", ylab = "RMSE", type = "l", xaxs = "i", lwd = 2)
      polygon(x = c(bandwidths, rev(bandwidths)), y = c(mean + se, rev(mean - se)), col = adjustcolor("#0E4749", alpha.f = 0.3), border = FALSE)
      grid(col=1)
      #points(x = hyperparameters[as.character(depth),variable], y = min(mean), pch = 21, bg = "white", lwd = 2, xpd = NA)
      title(main = paste("Variable:", variable, "   Depth:", depth))
   }
}
dev.off()


#' -----------------------------------------------------------------------------------------------------------
#' Explanatory GIF
#' -----------------------------------------------------------------------------------------------------------

ani.options(ani.width = 1600, # setting width
            ani.height = 1000, # setting height
            interval = 0.05,
            ani.res = 300) # setting time/image
saveGIF(expr = {
   s <- exp(seq(log(100),log(3),length=100))
   for (bandwidth in c(s,rev(s))) {
      layout(matrix(c(1,1,1,1,3,2,2,2,2,3), ncol = 2))
      par(mar = rep(0.01, 4), oma = rep(0.01, 4))
      formula <- as.formula(paste("gN2ONha", "date", sep  = " ~ "))
      subset <- na.omit(data[data$column==4 & data$depth==120,c("date","gN2ONha")])
      subset[,"date"] %<>% as.numeric
      plot(formula, data = subset, log = "", xlab = "", ylab = "", cex = 0.6, axes = FALSE); box()
      model <- npreg(formula, data = subset, bws = bandwidth)
      dates <- seq.Date(from = as.Date("2015-08-01"), to = as.Date("2016-02-28"), by = 1)
      x <- as.numeric(dates)
      y <- predict(model, newdata = data.frame(date = x), se.fit = TRUE)
      for (sigma in 1:3) {
         polygon(c(x, rev(x)), c(y$fit+y$se.fit*sigma, rev(y$fit-y$se.fit*sigma)), col = adjustcolor("#0E4749", alpha.f = 0.25), border = FALSE)
      }
      lines(dates, y$fit, lwd = 2)
      points(formula, data = subset, lwd = 1.5, cex = 0.6)

      formula <- as.formula(paste("SP", "date", sep  = " ~ "))
      subset <- na.omit(data[data$column==1 & data$depth==7.5,c("date","SP")])
      subset[,"date"] %<>% as.numeric
      plot(formula, data = subset, log = "", xlab = "", ylab = "", cex = 0.6, axes = FALSE); box()
      model <- npreg(formula, data = subset, bws = bandwidth)
      dates <- seq.Date(from = as.Date("2015-08-01"), to = as.Date("2016-02-28"), by = 1)
      x <- as.numeric(dates)
      y <- predict(model, newdata = data.frame(date = x), se.fit = TRUE)
      for (sigma in 1:3) {
         polygon(c(x, rev(x)), c(y$fit+y$se.fit*sigma, rev(y$fit-y$se.fit*sigma)), col = adjustcolor("#0E4749", alpha.f = 0.25), border = FALSE)
      }
      lines(dates, y$fit, lwd = 2)
      points(formula, data = subset, lwd = 1.5, cex = 0.6)
      
      par(mar = c(4,0.5,0,0.5))
      plot(NA, ylim = c(0,1), xlim = c(1,200), axes = FALSE, ylab = "", xlab = "Bandwidth", log = "x", yaxs = "i")
      axis(1)
      points(x = bandwidth, y = 0, xpd = NA, pch = 16)
   }},
   movie.name = "explanation.gif"
)
system("mv explanation.gif results/explanation.gif")

#' -----------------------------------------------------------------------------------------------------------
#' Hyperparameter bubble plot
#' -----------------------------------------------------------------------------------------------------------

variablenames <- c(expression("N"[2]*"O concentration"), "Site preference", expression(delta^{18}*"O"))
for (v in 1:3) {
   svg(paste("results/hyperparameters-", v, ".svg", sep = ""), width = 7, height = 4)
   plot(NA, xlim = c(0.5,12.5), ylim = c(0.5,5.5), axes = FALSE, ylab = "Depth [cm]", xlab = "Column", main = variablenames[v])
   abline(h=1:5, v=1:12, lty = 3)
   axis(1, at = 1:12, labels = 1:12)
   axis(2, las = 1, at = 1:5, labels = depths)
   col <- palette(12 * 5)
   box()
   for (i in 1:12) {
      for (j in 1:5) {
         hyperparameters[j,i,v]
         points(x = i, y = j, cex = 1.1*log(hyperparameters[j,i,v]), pch = 21, bg = colorRampPalette(c("red","#66999B"))(31)[round(10*(log(hyperparameters))-15)[j,i,v]])
      }
   }
   dev.off()
}

#' -----------------------------------------------------------------------------------------------------------
#' Curve fitting
#' -----------------------------------------------------------------------------------------------------------

pdf("results/curve-fitting.pdf", height = 12*2, width = 5*2.5)
variables <- c("gN2ONha", "SP", "d18O")
for (v in 1:length(variables)) {
   variable <- variables[v]
   par(oma = c(2,8,10,2), mar = c(0.5,3,0.5,0), mfrow = c(12, 5))
   for (column in 1:12) {
      for (depth in depths) {
         
         # Formula for this plot
         formula <- as.formula(paste(variable, "date", sep  = " ~ "))
         
         # Subset for this plot
         subset <- na.omit(data[data$column==column & data$depth==depth,c("date",variable)])
         subset[,"date"] %<>% as.numeric
         
         # Start plotting plane
         plot(formula, data = subset, log = "", xlab = "", ylab = "", cex = 0.6, axes = FALSE); box(); axis(2)
         
         # Fit a kernel regression model
         bandwidth <- hyperparameters[as.character(depth),column,variable]
         model <- npreg(formula, data = subset, bws = bandwidth)
         
         # Add model visualization
         dates <- seq.Date(from = as.Date("2015-08-01"), to = as.Date("2016-02-28"), by = 1)
         x <- as.numeric(dates)
         y <- predict(model, newdata = data.frame(date = x), se.fit = TRUE)
         for (sigma in 1:3) {
            polygon(c(x, rev(x)), c(y$fit+y$se.fit*sigma, rev(y$fit-y$se.fit*sigma)), col = adjustcolor(palette(length(variables)+1)[v], alpha.f = 0.25), border = FALSE)
         }
         lines(dates, y$fit, lwd = 2)
         points(formula, data = subset, lwd = 1.5, cex = 0.6)
         grid(col=1)
         legend("topright", bty = "n", legend = paste("BW =", signif(bandwidth,3)), text.font = 2)
         
         # Add axes
         if(column==1 & depth %in% depths) {
            axis(3, at = c(16700,16750,16800), labels = c("Sep", "Nov", "Jan"))
         } else if(column==12 & depth %in% depths) {
            axis(1, at = c(16700,16750,16800), labels = c("Sep", "Nov", "Jan"))
         }
         if(depth==7.5 & column %in% 1:12) {
            title(ylab = expression("N"[2]*"O-N concentration [g ha"^{-2}*"]"), xpd = NA)
         }
      }
   }
   mtext(paste(depths, "cm"), 3, outer = TRUE, adj = c(0.1,0.3,0.5,0.7,0.9)*1.03, line = 3.5)
   mtext("Depth", 3, outer = TRUE, line = 6, cex = 1.2, font = 2)
   mtext(12:1, 2, outer = TRUE, adj = (1:12)/12-0.042, line = 3.5)
   mtext("Column", 2, outer = TRUE, line = 6, cex = 1.2, font = 2)
   mtext(variable, 3, outer = TRUE, line = 6, cex = 1.5, font = 2, adj = -0.05)
}
dev.off()

# Less flexible option
hyperparameters <- apply(hyperparameters, 3, function(x) exp(mean(log(x))))
pdf("results/curve-fitting-fixed.pdf", height = 12*2, width = 5*2.5)
variables <- c("gN2ONha", "SP", "d18O")
for (v in 1:length(variables)) {
   variable <- variables[v]
   par(oma = c(2,8,10,2), mar = c(0.5,3,0.5,0), mfrow = c(12, 5))
   for (column in 1:12) {
      for (depth in depths) {
         
         # Formula for this plot
         formula <- as.formula(paste(variable, "date", sep  = " ~ "))
         
         # Subset for this plot
         subset <- na.omit(data[data$column==column & data$depth==depth,c("date",variable)])
         subset[,"date"] %<>% as.numeric
         
         # Start plotting plane
         plot(formula, data = subset, log = "", xlab = "", ylab = "", cex = 0.6, axes = FALSE); box(); axis(2)
         
         # Fit a kernel regression model
         bandwidth <- hyperparameters[variable]
         model <- npreg(formula, data = subset, bws = bandwidth)
         
         # Add model visualization
         dates <- seq.Date(from = as.Date("2015-08-01"), to = as.Date("2016-02-28"), by = 1)
         x <- as.numeric(dates)
         y <- predict(model, newdata = data.frame(date = x), se.fit = TRUE)
         for (sigma in 1:3) {
            polygon(c(x, rev(x)), c(y$fit+y$se.fit*sigma, rev(y$fit-y$se.fit*sigma)), col = adjustcolor(palette(length(variables)+1)[v], alpha.f = 0.25), border = FALSE)
         }
         lines(dates, y$fit, lwd = 2)
         points(formula, data = subset, lwd = 1.5, cex = 0.6)
         grid(col=1)

         # Add axes
         if(column==1 & depth %in% depths) {
            axis(3, at = c(16700,16750,16800), labels = c("Sep", "Nov", "Jan"))
         } else if(column==12 & depth %in% depths) {
            axis(1, at = c(16700,16750,16800), labels = c("Sep", "Nov", "Jan"))
         }
         if(depth==7.5 & column %in% 1:12) {
            title(ylab = expression("N"[2]*"O-N concentration [g ha"^{-2}*"]"), xpd = NA)
         }
      }
   }
   mtext(paste(depths, "cm"), 3, outer = TRUE, adj = c(0.1,0.3,0.5,0.7,0.9)*1.03, line = 3.5)
   mtext("Depth", 3, outer = TRUE, line = 6, cex = 1.2, font = 2)
   mtext(12:1, 2, outer = TRUE, adj = (1:12)/12-0.042, line = 3.5)
   mtext("Column", 2, outer = TRUE, line = 6, cex = 1.2, font = 2)
   mtext(variable, 3, outer = TRUE, line = 6, cex = 1.5, font = 2, adj = -0.05)
}
dev.off()

#' ANOVA?
df <- as.data.frame.table(hyperparameters, responseName = c("bandwidth"))
colnames(df) <- c("depth","column","variable","bandwidth")
lm(bandwidth ~ depth * column + variable, data = df) %>% anova()

