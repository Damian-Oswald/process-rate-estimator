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

#' -----------------------------------------------------------------------------------------------------------
#' Define global variables
#' -----------------------------------------------------------------------------------------------------------

palette <- colorRampPalette(c("#66999B","#0E4749","#E55812","#EFE7DA"))

#' -----------------------------------------------------------------------------------------------------------
#' Set Model Parameters
#' -----------------------------------------------------------------------------------------------------------

#' Different depths
depths <- c(7.5, 30, 60, 90, 120)

#' Bulk density, average across all depths and columns.
BD <- 1.686
theta_t <- 1- BD/2.65
temperature <- 298
D_fw <- (5.07e-6) * exp(-2371/temperature)
D_fa <- 0.1436e-4 * (temperature/273.15)^1.81
H <- (8.5470e6 * exp(-2284/temperature)) / (8.3145*temperature)
rho <- 1.26e6
N2Oatm <- 0.2496 # ppmv; atmospheric N2O concentration

#' -----------------------------------------------------------------------------------------------------------
#' Load and prepare the data
#' -----------------------------------------------------------------------------------------------------------

#' Read the prepared data
data <- read.csv("data/data.csv")

#' Transform the variable `date_R` to `Date` class
data[,"date"] %<>% as.Date

#' Transform `column` to `ordered` to `integer`
data[,"column"] %<>% as.ordered

#' Transform `variety` from `character` to `factor`
data[,"variety"] %<>% as.factor()

#' -----------------------------------------------------------------------------------------------------------
#' Start calculating the parameters
#' -----------------------------------------------------------------------------------------------------------

#' Calculate average moisture across depths -- Soil volumetric water content `theta_w`
#' We calculate the following: for every column, day, depth,
#' calculate the mean depth of this one and the one above --
#' except for the top depth, there, return single value.
data$theta_w <- numeric(nrow(data))
for (i in 1:nrow(data)) {
   j <- which(depths==data[i,"depth"]) # Get the current depth index
   data[i,"theta_w"] <- mean(data[data[,"column"]==data[i,"column"] & data[,"date"]==data[i,"date"] & data[,"depth"]%in%depths[(j-1):j], "moisture"])
   # TODO: Build checker to see if calculation is correct, no missing values, always 1 or 2 values etc.
}

#' Calculate air-filled pore spaces across depths
data$theta_a <- 1 - data$theta_w/theta_t

#' Calculate Ds
data$Ds <- with(data, ((theta_w^(10/3)*D_fw)/H+theta_a^(10/3)*D_fa)*theta_t^-2)

#' *Calculate dC/dz*
#' In this step, we calculate the N2O concentration gradient as the difference in concentration between the current depth
#' which is saved as `N2O_bottom` and the one on top of the current depth which is `N2O_top`.
data$dCdZ <- numeric(nrow(data))
for (i in 1:nrow(data)) {
   j <- which(depths==data[i,"depth"]) # Get the current depth index
   current_date_column <- data[,"column"]==data[i,"column"] & data[,"date"]==data[i,"date"]
   
   N2O_top <- if(j==1){
      N2O_top <- N2Oatm
   } else {
      N2O_top <- data[current_date_column & data[,"depth"]==depths[j-1], "corrected.N2O"]
   }
   N2O_bottom <- data[current_date_column & data[,"depth"]==depths[j], "corrected.N2O"]
   
   # If any observation is missing, skip to the next
   if(length(N2O_bottom)==0 | length(N2O_top)==0) next
   
   # TODO: The `.Rmd` file contains the following code:
   #
   # data_C1$dCdz_top_7 <- (data_C1$corrected.N2O_7 - N2Oatm)/1000000/(15/100/2)
   # data_C1$dCdz_top_30 <- (data_C1$corrected.N2O_30 - data_C1$corrected.N2O_7)/1000000/(30/100/2+15/100/2)
   # data_C1$dCdz_top_60 <- (data_C1$corrected.N2O_60 - data_C1$corrected.N2O_30)/1000000/(30/100/2+30/100/2)
   # data_C1$dCdz_top_90 <- (data_C1$corrected.N2O_90 - data_C1$corrected.N2O_60)/1000000/(30/100/2+30/100/2)
   # data_C1$dCdz_top_120 <- (data_C1$corrected.N2O_120 - data_C1$corrected.N2O_90)/1000000/(30/100/2+30/100/2)
   #
   # Figure out how the values in the denominator came to be, and change code accoringly.
   denominator <- c(`7.5` = 15/100/2, `30` = 30/100/2+15/100/2, `60` = 30/100/2+30/100/2, `90` = 30/100/2+30/100/2, `120` = 30/100/2+30/100/2)
   
   # TODO: Adjust the code if calculation is incorrect
   data[current_date_column & data[,"depth"]==depths[j], "dCdZ"] <- (N2O_bottom - N2O_top)/1000000/denominator[j]
   
   # TODO: Build checker to see if calculation is correct, no missing values, always 1 or 2 values etc.
}

#' Do we want this here?
top <- c(0,depths)[-6]
bottom <- depths
sapply(1:5, function(i) 1000000/(bottom[i]/100/2+top[i]/100/2))

#' Calculate `F_calc` across depths, considering flux is upward 
#' originally in mg N2O-N/m2/s, converted to g N/ha/day
data$F_calc <- with(data, dCdZ * Ds * rho * 10000 * 3600 * 24/1000)

#' Calculate F_in and F_out from the bottom and top boundary of each layer
data$F_top_in <- data$F_top_out <- data$F_bottom_in <- data$F_bottom_out <- numeric(nrow(data))
for (i in 1:nrow(data)) {
   j <- which(depths==data[i,"depth"]) # Get the current depth index
   current_date_column <- data[,"column"]==data[i,"column"] & data[,"date"]==data[i,"date"]
   
   F_calc_top <- data[current_date_column & data$depth == depths[j],"F_calc"]
   if(j==5) F_calc_bottom <- 0
   else F_calc_bottom <- data[current_date_column & data$depth == depths[j+1],"F_calc"]
   if(length(F_calc_top)==0) F_calc_top <- NA
   if(length(F_calc_bottom)==0) F_calc_bottom <- NA
   
   data[current_date_column & data$depth == depths[j],"F_top_in"] <- ifelse(F_calc_top > 0, 0, abs(F_calc_top))
   data[current_date_column & data$depth == depths[j],"F_top_out"] <- ifelse(F_calc_top < 0, 0, abs(F_calc_top))
   data[current_date_column & data$depth == depths[j],"F_bottom_in"] <- ifelse(F_calc_bottom < 0, 0, abs(F_calc_bottom))
   data[current_date_column & data$depth == depths[j],"F_bottom_out"] <- ifelse(F_calc_bottom > 0, 0, abs(F_calc_bottom))
   
}

column <- 1
for (depth in depths) {
   data[data$depth==depth & data$column==column,c("date","F_top_out")] |> na.omit() |> plot(ylim = c(0,30), type = "o", lty = 2, ylab = "Flux")
   data[data$depth==depth & data$column==column,c("date","F_top_in")] |> na.omit() |> points(col = "red", type = "o", lty = 2)
   data[data$depth==depth & data$column==column,c("date","F_bottom_out")] |> na.omit() |> points(col = "blue", type = "o", lty = 2)
   data[data$depth==depth & data$column==column,c("date","F_bottom_in")] |> na.omit() |> points(col = "orange", type = "o", lty = 2)
   title(main = paste("Depth =",depth,"cm"))
}

#' TODO: The loops can be more efficient by combining all the steps at once, and by looking only at observations with N2O values.
#' For this, check first if `gN2ONha` is `NA`

#' In n' out
data$F_out <- with(data, F_bottom_out + F_top_out)


#' -----------------------------------------------------------------------------------------------------------
#' Determine infinitesimal changes in concentrations and natural abundance isotope
#' values of N2O as the first derivative of a smooth curve
#' -----------------------------------------------------------------------------------------------------------

#' - The curves are fitted for N2O concentrations, SP and d18O
pdf("results/Curves.pdf", height = 12*2, width = 5*2.5)
bandwidth <- 12
for (variable in c("gN2ONha", "SP", "d18O")) {
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
         model <- npreg(formula, data = subset, bws = bandwidth)
         
         dates <- seq.Date(from = as.Date("2015-08-01"), to = as.Date("2016-02-28"), by = 1)
         x <- as.numeric(dates)
         y <- predict(model, newdata = data.frame(date = x), se.fit = TRUE)
         for (sigma in 1:3) {
            polygon(c(x, rev(x)), c(y$fit+y$se.fit*sigma, rev(y$fit-y$se.fit*sigma)), col = adjustcolor(palette(1), alpha.f = 0.25), border = FALSE)
         }
         lines(dates, y$fit, lwd = 2)
         points(formula, data = subset, lwd = 1.5, cex = 0.6)
         grid(col=1)
         
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
   mtext(1:12, 2, outer = TRUE, adj = (1:12)/12-0.042, line = 3.5)
   mtext("Column", 2, outer = TRUE, line = 6, cex = 1.2, font = 2)
   mtext(variable, 3, outer = TRUE, line = 6, cex = 1.5, font = 2, adj = -0.05)
}
dev.off()

model <- function(x_train, x_test, y_train, y_test, ...) {
   train <- cbind(x_train, y_train)
   test <- cbind(x_test, y_test)
   model <- npreg(formula, data = subset, ...)
}

i <- sample(nrow(subset))[1:5]
x_train <- subset[-i,"date"]
x_test <- subset[i,"date"]
y_train <- subset[-i,variable]
y_test <- subset[-i,variable]

#' TODO: Change the ylab for both SP and d18O
#' Idea: Use Cross-Validation to find best bandwidth for the fit in kernel regression, but only use one single hyperparameter for all the fits!

