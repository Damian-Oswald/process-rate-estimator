#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-02

#' Define global variables
colors_variety <- viridis::mako(4)

data <- read.csv("data/three-way-data.csv", row.names = 1)


head(data)
dim(data)
data$date_R
unique(data$date_R)

as.factor(paste(data$column,data$date_R,sep="_"))

#' Transform the variable `date_R` to `Date` class
#' QUESTION: Why isn't it correct unless I subtract 1 from the DOY?
data$date <- as.Date(as.integer(substr(data$date_R, 5, 7))-1, origin = paste0(as.integer(substr(data$date_R, 1, 4)),"-01-01"))

#' There is C:N ratio that is `Inf` -- should probably be omitted?
data[which(data$CN==Inf),"CN"] <- NA

#' First look at the data
boxplot(moisture ~ depth, data, outline = FALSE)
boxplot(moisture ~ variety, data)
boxplot(moisture ~ increment, data) # What is "increment"?
with(data, interaction.plot(date, depth, moisture)) # Interpretation: the soil moisture starts out relatively homogeneous, but as plants grow, it starts to differ by depth
for (y in colnames(data)[8:21]) {
   log <- ifelse(min(data[,y],na.rm=TRUE)>0,"x","")
   boxplot(data[,y] ~ variety * I(-depth), data, log = log, outline = FALSE,
           col = rep(colors_variety,times=5),xlab=y,
           names = -rep(sort(unique(data$depth),decreasing = T),each=4),
           horizontal = TRUE)
}
for (y in colnames(data)[8:21]) {
   log <- ifelse(min(data[,y],na.rm=TRUE)>0,"y","")
   with(data, interaction.plot(depth, variety, data[,y], ylab = y, log = log, fun = function(x) mean(x,na.rm=TRUE))) # Interpretation: the soil moisture starts out relatively homogeneous, but as plants grow, it starts to differ by depth
}








