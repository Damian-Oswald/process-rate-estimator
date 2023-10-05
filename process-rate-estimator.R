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
theta_T <- 1- BD/2.65
temperature <- 298
Dfw <- (5.07e-6)*exp(-2371/temperature)
Dfa <- 0.1436e-4*(temperature/273.15)^1.81
H <- (8.5470e6 * exp(-2284/temperature)) / (8.3145*temperature)
ro <- 1.26e6
N2Oatm <- 0.2496

#' -----------------------------------------------------------------------------------------------------------
#' Load and prepare the data
#' -----------------------------------------------------------------------------------------------------------

data <- read.csv("data/three-way-data.csv", row.names = 1)

#' Transform the variable `date_R` to `Date` class
#' QUESTION: Why isn't it correct unless I subtract 1 from the DOY?
data$date <- as.Date(as.integer(substr(data$date_R, 5, 7))-1, origin = paste0(as.integer(substr(data$date_R, 1, 4)),"-01-01"))

#' There is C:N ratio that is `Inf` -- should probably be omitted?
data[which(data$CN==Inf),"CN"] <- NA

#' -----------------------------------------------------------------------------------------------------------
#' Explore the data
#' -----------------------------------------------------------------------------------------------------------

#' First look at the data
boxplot(moisture ~ depth, data, outline = FALSE, col = palette(5))
boxplot(moisture ~ variety, data, col = palette(4))
boxplot(moisture ~ increment, data, col = palette(2)) # What is "increment"?
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

#' Mosaic plots -- of what data do we have entries?
par(mar=c(4,4,4,2)+0.1)
mosaicplot(~ depth + variety, data, col = palette(4)) # Why do we have the uneven distribution?
mosaicplot(~ depth + increment, data, col = palette(4))
mosaicplot(~ column + depth, data, col = palette(4))
mosaicplot(~ increment + variety, data, col = palette(4))

#' What data does have measurements?
x <- data[!is.na(data$SP),]
dim(x)
mosaicplot(~ column + depth, x, col = palette(4))
data[data$column=="C1",] |> dim()

#' Calculate average moisture across depths
x <- with(data, tapply(moisture, list(depth), mean))

#' Calculate mean moisture between the depths (?)
sapply(1:5, function(i) mean(x[(i-1):i]))

#' We can also cross multiple factors
with(data, tapply(moisture, list(depth, variety), mean))








data_C1_7 <- subset(data, depth == "7.5" & column == "C1")
names(data_C1_7) <- c("day_column_depth","date_R", "column","depth","increment","column","moisture_7","corrected.N2O_7","gN2ONha_7","d15Nbulk_7","SP_7", "d18O_7","column_date")
data_C1_30 <- subset(data, depth == "30" & column == "C1")
names(data_C1_30) <- c("day_column_depth","date_R", "column","depth","increment","column","moisture_30","corrected.N2O_30","gN2ONha_30","d15Nbulk_30","SP_30", "d18O_30","column_date")
data_C1_60 <- subset(data, depth == "60" & column == "C1")
names(data_C1_60) <- c("day_column_depth","date_R", "column","depth","increment","column","moisture_60","corrected.N2O_60","gN2ONha_60","d15Nbulk_60","SP_60", "d18O_60","column_date")
data_C1_90 <- subset(data, depth == "90" & column == "C1")
names(data_C1_90) <- c("day_column_depth","date_R", "column","depth","increment","column","moisture_90","corrected.N2O_90","gN2ONha_90","d15Nbulk_90","SP_90", "d18O_90","column_date")
data_C1_120 <- subset(data, depth == "120" & column == "C1")
names(data_C1_120) <- c("day_column_depth","date_R", "column","depth","increment","column","moisture_120","corrected.N2O_120","gN2ONha_120","d15Nbulk_120","SP_120", "d18O_120","column_date")




data_C1_7$theta_w_top_7

data_C1_7$moisture_7

#calculate average moisture across depths

data_C1_7$moisture_7
(data_C1_7$moisture_7 + data_C1_30$moisture_30)/2

#' What do we want to do in this step? 
#' For every day in `date` (and apparently only for `column == "C1"`, why?),
#' we want to calculate average moisture across depths

mosaicplot(~ variety + column, data)

mosaicplot(~ variety, data = data[data$column=="C1" & data$depth==7.5,])


for (i in 1:length(depths)) {
   upper <- data[data$column=="C1" & data$depth==depths[i-1],"moisture"]
   lower <- data[data$column=="C1" & data$depth==depths[i],"moisture"]
   if(length(upper)==0) upper <- 0
   x <- (upper + lower)/2
   plot(x ~ na.omit(unique(data$date)), xlab = "Date", ylab = "Avg. moisture")
}





