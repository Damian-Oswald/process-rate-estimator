#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-02
#' ---

#' -----------------------------------------------------------------------------------------------------------
#' Prepare work space
#' -----------------------------------------------------------------------------------------------------------

#' Attach packages to search path
library(magrittr)

#' Define color function for plots
palette <- colorRampPalette(c("#66999B","#0E4749","#E55812","#EFE7DA"))

#' Attach parameter list to global environment
PRE::getParameters() |> attach()

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
data[,"variety"] %<>% as.factor

#' -----------------------------------------------------------------------------------------------------------
#' Calculate N2O-N 
#' -----------------------------------------------------------------------------------------------------------

#' Calculate the volumetric N2O-N
data[,"N2ONvolume"] <- with(data, corrected.N2O * 1/(R*temperature)*28)

#' Calculate the area N2O-N
data[,"N2ONarea"] <- with(data, N2ONvolume * increment/100 * (theta_t - moisture) * 10000/1000)

#' -----------------------------------------------------------------------------------------------------------
#' Start calculating the parameters
#' -----------------------------------------------------------------------------------------------------------

#' Calculate average moisture across depths -- Soil volumetric water content `theta_w`
#' We calculate the following: for every column, day, depth: calculate the mean depth of this one and the one above --
#' except for the top depth, there, return single value.
data$theta_w <- numeric(nrow(data))
for (i in 1:nrow(data)) {
   j <- which(depths==data[i,"depth"]) # Get the current depth index
   data[i,"theta_w"] <- mean(data[data[,"column"]==data[i,"column"] & data[,"date"]==data[i,"date"] & data[,"depth"]%in%depths[(j-1):j], "moisture"]) # Calculate the mean of the current moisture and the one above (if it is the top layer, have the mean of one)
   # TODO: Build checker to see if calculation is correct, no missing values, always 1 or 2 values etc.
}

#' Calculate air-filled pore spaces across depths
data$theta_a <- with(data, 1 - theta_w/theta_t)

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

#' Write data to resources
write.csv(data, file = "data/data-calculated.csv", row.names = FALSE)

