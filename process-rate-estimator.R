#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-02

#' -----------------------------------------------------------------------------------------------------------
#' Define global variables
#' -----------------------------------------------------------------------------------------------------------

palette <- colorRampPalette(c("#66999B","#0E4749","#E55812","#EFE7DA"))

#' -----------------------------------------------------------------------------------------------------------
#' Set Model Parameters
#' -----------------------------------------------------------------------------------------------------------

#' Different depths
depths <- c(7.5,30,60,90,120)

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

data <- read.csv("data/three-way-data.csv", row.names = 1)

#' Transform the variable `date_R` to `Date` class
#' We have to subtract 1 from the cut out DOY, because the as.Date function starts counting from 0 onwards...
data$date <- as.Date(as.integer(substr(data$date_R, 5, 7))-1, origin = paste0(as.integer(substr(data$date_R, 1, 4)),"-01-01"))
#' Some dates are missing. Proposition: The "true" date should be "2016-02-03"
data[is.na(data[,"date"]),"date"] <- as.Date("2016-02-03")

#' Transform `variety` from `character` to `factor`
data[,"variety"] |> as.factor() -> data[,"variety"]

#' Transform `column` from `character` to `integer`
data[,"column"] |> substr(2, 3) |> as.integer() |> as.ordered() -> data[,"column"]

#' There is C:N ratio that is `Inf` -- should probably be omitted?
data[which(data$CN==Inf),"CN"] <- NA

#' -----------------------------------------------------------------------------------------------------------
#' Explore the data
#' -----------------------------------------------------------------------------------------------------------

#' The covered time span
diff(range(data$date, na.rm = T))

#' First look at the data
boxplot(moisture ~ depth, data, outline = FALSE, col = palette(5))
boxplot(moisture ~ variety, data, col = palette(4))
with(data, interaction.plot(date, depth, moisture)) # Interpretation: the soil moisture starts out relatively homogeneous, but as plants grow, it starts to differ by depth
for (y in colnames(data)[8:21]) {
   log <- ifelse(min(data[,y],na.rm=TRUE)>0,"x","")
   par(mar = c(4,4,0,0)+0.1)
   boxplot(data[,y] ~ variety * I(-depth), data, log = log, outline = FALSE,
           at = rep(1:4,times=5)+rep(0:4,each=4)*6,
           col = rep(palette(4),times=5),xlab=y,
           names = -rep(sort(unique(data$depth),decreasing = T),each=4),
           horizontal = TRUE,
           ylab = "depth",
           axes = FALSE,
           lty = 1)
   axis(1)
   axis(2, at = (0:4)*6+2.5, labels = c(120,90,60,30,7.5), las = 1)
   box()
}
for (y in colnames(data)[8:21]) {
   log <- ifelse(min(data[,y],na.rm=TRUE)>0,"y","")
   with(data, interaction.plot(depth, variety, data[,y], ylab = y, log = log, fun = function(x) mean(x,na.rm=TRUE))) # Interpretation: the soil moisture starts out relatively homogeneous, but as plants grow, it starts to differ by depth
}

#' Diffusion Fluxes
for (variety in unique(data$variety)) {
   with(data[(!is.na(data$corrected.N2O)) & (data$variety==variety),], interaction.plot(date, depth, corrected.N2O, col = par()$fg, ylab = expression("N"[2]*"O fluxes"), xlab = "Date", ylim = c(0,16)))
   title(main = paste("Diffusion fluxes of", variety))
}
for (variety in unique(data$variety)) {
   with(data[(!is.na(data$corrected.CO2)) & (data$variety==variety),], interaction.plot(date, depth, corrected.CO2, col = par()$fg, ylab = expression("CO"[2]*" fluxes"), xlab = "Date", ylim = c(0,45000), las = 0))
   title(main = paste("Diffusion fluxes of", variety))
}

#' Mosaic plots -- of what data do we have entries?
par(mar=c(4,4,4,2)+0.1)
mosaicplot(~ depth + variety, data, col = palette(4)) # Why do we have the uneven distribution? (Due to colunn 6)
mosaicplot(~ column + depth, data, col = palette(5)) # It is only with colunn 6! -> Yes, column 6 is missing observations.

#' What data does have measurements?
x <- data[!is.na(data$SP),]
mosaicplot(~ column + depth, x, col = palette(5), main = "data with measurements")
with(x, table(depth, column)) # Should be 24 each, no?
x$date |> unique() |> length() # Gas measurements only at 24 out of the dates
with(x, tapply(SP, date, length)) |> barplot(las=2, col = palette(24)); abline(h=60, lwd = 2, lty = 2) # This should all be 60 or not? (Why are there 1440 - 1117 = 323 entries missing?)
mosaicplot(~ column + depth, x, shade = TRUE, main = "data with measurements") # Why are there the differences?
with(x, table(date, column)) # We clearly see the missing values, not?

#' Pairs panel
# plot(data[,8:21], cex = 0.3)

#' Which two observations are missing in general
with(data, table(column, depth)) # What??? Shouldn't this be 160 each???
plot(moisture ~ date, data[data$column==6 & data$depth==90,], las = 0) # What happened with the moisture measurements for column 6 at depth = 90 cm?
#' Why do we have 160 measurements for column 1:4 and 161 for column 5:12?

#' What's going on here?
plot(gN2ONha ~ mgN2ONm3, data, col = palette(5)[as.factor(data$depth)], pch = 16, log = "xy")

#' NOTES OF THE DATA EXPLORATION
#' Some moisture observations are missing, specifically from column 6 at depth = 90 cm
#' Many observations are missing regarding the flux data
#' Fluxes were only measured at 24 distinct days (out of 161 possible days)

#' -----------------------------------------------------------------------------------------------------------
#' Start calculations
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
   
   data[current_date_column & data[,"depth"]==depths[j], "dCdZ"] <- (N2O_bottom - N2O_top)/1000000
   
   # TODO: The `.Rmd` file contains the following code:
   #
   # data_C1$dCdz_top_7 <- (data_C1$corrected.N2O_7 - N2Oatm)/1000000/(15/100/2)
   # data_C1$dCdz_top_30 <- (data_C1$corrected.N2O_30 - data_C1$corrected.N2O_7)/1000000/(30/100/2+15/100/2)
   # data_C1$dCdz_top_60 <- (data_C1$corrected.N2O_60 - data_C1$corrected.N2O_30)/1000000/(30/100/2+30/100/2)
   # data_C1$dCdz_top_90 <- (data_C1$corrected.N2O_90 - data_C1$corrected.N2O_60)/1000000/(30/100/2+30/100/2)
   # data_C1$dCdz_top_120 <- (data_C1$corrected.N2O_120 - data_C1$corrected.N2O_90)/1000000/(30/100/2+30/100/2)
   #
   # Figure out how the values in the denominator came to be, and change code accoringly.
   
   # TODO: Build checker to see if calculation is correct, no missing values, always 1 or 2 values etc.
}

#' Do we want this here?
top <- c(0,depths)[-6]
bottom <- depths
sapply(1:5, function(i) 1000000/(bottom[i]/100/2+top[i]/100/2))

#' Calculate `F_calc` across depths, considering flux is upward 
#' originally in mg N2O-N/m2/s, converted to g N/ha/day
data$F_calc <- with(data, dCdZ * Ds * rho * 10000 * 3600 * 24/1000)







