#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-16
#' ---

#' Read original data
data <- read.csv("data/three-way-data.csv", row.names = 1)

#' Transform the variable `date_R` to `Date` class
#' We have to subtract 1 from the cut out DOY, because the as.Date function starts counting from 0 onwards...
data$date <- as.Date(as.integer(substr(data$date_R, 5, 7))-1, origin = paste0(as.integer(substr(data$date_R, 1, 4)),"-01-01"))

#' Some dates are missing. Proposition: The actual date should be "2016-02-03" (?)
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

pdf("results/exploring-the-data.pdf", width = 8, height = 12)

par(mfrow = c(2,1))

#' Define color
palette <- colorRampPalette(c("#66999B","#0E4749","#E55812","#EFE7DA"))

#' The covered time span
diff(range(data$date, na.rm = T))

#' First look at the data
boxplot(moisture ~ depth, data, outline = FALSE, col = palette(5))
boxplot(moisture ~ variety, data, col = palette(4))
with(data, interaction.plot(date, depth, moisture)) # Interpretation: the soil moisture starts out relatively homogeneous, but as plants grow, it starts to differ by depth
for (y in colnames(data)[8:21]) {
   log <- ifelse(min(data[,y],na.rm=TRUE)>0,"x","")
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
   with(data[(!is.na(data$N2O)) & (data$variety==variety),], interaction.plot(date, depth, N2O, col = par()$fg, ylab = expression("N"[2]*"O fluxes"), xlab = "Date", ylim = c(0,16)))
   title(main = paste("Diffusion fluxes of", variety))
}
for (variety in unique(data$variety)) {
   with(data[(!is.na(data$CO2)) & (data$variety==variety),], interaction.plot(date, depth, CO2, col = par()$fg, ylab = expression("CO"[2]*" fluxes"), xlab = "Date", ylim = c(0,45000), las = 0))
   title(main = paste("Diffusion fluxes of", variety))
}

#' Mosaic plots -- of what data do we have entries?
mosaicplot(~ depth + variety, data, col = palette(4)) # Why do we have the uneven distribution? (Due to colunn 6)
mosaicplot(~ column + depth, data, col = palette(5)) # It is only with colunn 6! -> Yes, column 6 is missing observations.

#' What data does have measurements?
x <- data[!is.na(data$gN2ONha),]
mosaicplot(~ column + depth, x, col = palette(5), main = "data with measurements")
with(x, table(depth, column)) # Should be 24 each, no?
x$date |> unique() |> length() # Gas measurements only at 24 out of the dates
with(x, tapply(SP, date, length)) |> barplot(las=2, col = palette(24)); abline(h=60, lwd = 2, lty = 2) # This should all be 60 or not? (Why are there 1440 - 1117 = 323 entries missing?)
mosaicplot(~ column + depth, x, shade = TRUE, main = "data with measurements") # Why are there the differences?
with(x, table(date, column)) # We clearly see the missing values, not?

#' Pairs panel
plot(data[,8:21], cex = 0.3)

#' Which two observations are missing in general
with(data, table(column, depth)) # What??? Shouldn't this be 160 each???
plot(moisture ~ date, data[data$column==6 & data$depth==90,], las = 0, pch = 16, main = "Moisture for the missing depth and column") # What happened with the moisture measurements for column 6 at depth = 90 cm?
#' Why do we have 160 measurements for column 1:4 and 161 for column 5:12?

#' What's going on here?
plot(gN2ONha ~ mgN2ONm3, data, col = palette(5)[as.factor(data$depth)], pch = 16, log = "xy")

#' Visualize the missing data
naniar::vis_miss(data)

#' Visualize the correlation of the missing values
apply(data, 2, function(x) is.na(x))[,-c(1:7,22)] |> cor() |> corrplot::corrplot(type = "upper", tl.col = par()$fg, method = "pie")

#' All the missing data have the same

dev.off()

#' NOTES OF THE DATA EXPLORATION
#' Some moisture observations are missing, specifically from column 6 at depth = 90 cm
#' Many observations are missing regarding the flux data
#' Fluxes were only measured at 24 distinct days (out of 161 possible days)

#' -----------------------------------------------------------------------------------------------------------
#' Prepare data for later use
#' -----------------------------------------------------------------------------------------------------------

#' Rearrange the data frame with only relevant variables
measurements <- data[,c("date", "column", "depth", "increment", "variety", "moisture", "N2O", "CO2", "SP", "d18O", "d15Nbulk", "d15Nalpha", "d15Nbeta")]

#' Set package working directory
setwd(file.path(getwd(),"PRE"))

#' Save `measurements` data frame for the R package `PRE`
usethis::use_data(measurements, overwrite = TRUE)


