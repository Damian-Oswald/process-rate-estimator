#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-16
#' ---

#' Read original data
data <- read.csv(file.path("scripts","data-preparation","data","three-way-data.csv"), row.names = 1)

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

#' Mosaic plots -- of what data do we have entries?
# mosaicplot(~ depth + column, data, shade = TRUE) # Missing data for column 6 at depth 90 cm

#' Which two observations are missing in general
with(data, table(column, depth)) # This should be 160 or 161 each...

#' The moisture sensor was not measuring for three months...
# plot(moisture ~ date, data[data$column==6 & data$depth==90,], type = "o") # What happened with the moisture measurements for column 6 at depth = 90 cm?

#' However, this is not so bad after all -- if we adjust the moisture axis range
# plot(moisture ~ date, data[data$column==6 & data$depth==90,], type = "o", ylim = c(0,0.35)) # What happened with the moisture measurements for column 6 at depth = 90 cm?

#' NOTES OF THE DATA EXPLORATION
#' Some moisture observations are missing, specifically from column 6 at depth = 90 cm
#' We should be able to interpolate the missing values

#' -----------------------------------------------------------------------------------------------------------
#' Interpolate moisture data
#' -----------------------------------------------------------------------------------------------------------

# find the missing days
missing_days <- unique(data[,"date"])[!(unique(data[,"date"]) %in% data[data$column==6 & data$depth==90,c("date")])]

# generate clone for the missing days
clone <- data[data$column==6 & data$depth==7.5 & data$date %in% missing_days,]
clone[,7:21] <- NA
clone[,"depth"] <- 90
clone[,"moisture"] <- predict(loess(moisture ~ as.integer(date), data[data$column==6 & data$depth==90,c("date","moisture")]), newdata = as.integer(missing_days))

# combine data with the interpolated data
newdata <- rbind(data, clone)

# check number of days for each column * depth
with(newdata, tapply(date, list(depth, column), length))

# sort the new data frame
newdata <- newdata[with(newdata, order(date, column, depth)),]

#' -----------------------------------------------------------------------------------------------------------
#' Prepare data for later use
#' -----------------------------------------------------------------------------------------------------------

#' Rearrange the data frame with only relevant variables
measurements <- newdata[,c("date", "column", "depth", "increment", "variety", "moisture", "N2O", "SP", "d18O")]

#' Set package working directory
# setwd("<PATH TO PRE PACKAGE DIRECTORY")

#' Save `measurements` data frame for the R package `PRE`
# usethis::use_data(measurements, overwrite = TRUE)
