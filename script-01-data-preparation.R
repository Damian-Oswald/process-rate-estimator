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
library(PRE)

#' Define color function for plots
palette <- colorRampPalette(c("#66999B","#0E4749","red"))

#' Attach parameter list to global environment
PRE::getParameters() |> attach()

#' Read the prepared data
data <- PRE::measurements

#' Get information on data
help("measurements")

#' Save index vector of complete observations
data |> apply(2, is.na) |> apply(1, any) |> invert() |> which() -> complete

#' -----------------------------------------------------------------------------------------------------------
#' Calculate N2O-N 
#' -----------------------------------------------------------------------------------------------------------

#' Calculate the volumetric N2O-N
data[,"N2ONvolume"] <- with(data, N2O * 1/(R*temperature)*28)

#' Calculate the area N2O-N
data[,"N2ONarea"] <- with(data, N2ONvolume * increment/100 * (theta_t - moisture) * 10000/1000)

visdat::vis_miss(data[complete,-c(1:13)])

visdat::vis_miss(PRE::measurements[,-c(8,11:13)])

#' -----------------------------------------------------------------------------------------------------------
#' Start calculating the parameters
#' -----------------------------------------------------------------------------------------------------------

#' Calculate average moisture across depths -- Soil volumetric water content `theta_w`
#' We calculate the following: for every column, day, depth: calculate the mean depth of this one and the one above --
#' except for the top depth, there, return single value.
for (i in complete) {
   
   # Create a `mask`, which is a logical vector selecting all current 
   mask <- data[,"column"]==data[i,"column"] & data[,"date"]==data[i,"date"] # create a mask for selecting the right subset of the data
   j <- which(depths==data[i,"depth"]) # Get the current depth index
   
   # Calculate the mean of the current depth moisture and the one above (if it is the top layer, that's the mean of one observation)
   data[i,"theta_w"] <- mean(data[mask & data[,"depth"] %in% depths[(j-1):j], "moisture"])
}

#' Calculate air-filled pore spaces across depths
data$theta_a <- with(data, 1 - theta_w/theta_t)

#' Calculate Ds
data$Ds <- with(data, ((theta_w^(10/3)*D_fw)/H+theta_a^(10/3)*D_fa)*theta_t^-2)

visdat::vis_miss(data[complete,-c(1:13)])

#' *Calculate dC/dz*
#' In this step, we calculate the N2O concentration gradient as the difference in concentration between the current depth increment and the one above
for (i in complete) {
   
   # Create a `mask`, which is a logical vector selecting all current 
   j <- which(depths==data[i,"depth"]) # Get the current depth index
   mask <- with(data, column == data[i,"column"] & date == data[i,"date"] & depth %in% depths[(j-1):j])
   
   # Get the N2O concentration of current and above layer
   concentration <- data[mask,"N2O"]
   if(j==1) concentration <- c(N2Oatm, concentration)
   
   # Save the denominator, which is the distance from one measurement point to the next
   denominator <- (diff(c(0,depths))/100)[j]
   value <- diff(concentration)/1000000/denominator
   data[i,"dCdZ"] <- ifelse(length(value)==1, value, NA)
   
}

#' Calculate `F_calc` across depths, considering flux is upward (originally in mg N2O-N/m2/s, converted to g N/ha/day)
data[,"F_calc"] <- with(data, dCdZ * Ds * rho * 10000 * 3600 * 24/1000)

#' Check missing values
visdat::vis_miss(data[complete,-c(1:13)])
data[complete,][is.na(data[complete,"F_bottom_in"]),]
#' Missing values for column 6 in the lowest depth due to missing increment measurements...

#' Calculate F_in and F_out from the bottom and top boundary of each layer
for (i in complete) {
   
   # Create a `mask`, which is a logical vector selecting all current 
   j <- which(depths==data[i,"depth"]) # Get the current depth index

   # Save indices of the current increment (center), the one on top and the one on bottom
   top <- which(with(data, column == data[i,"column"] & date == data[i,"date"] & depth == depths[j-1]))
   center <- which(with(data, column == data[i,"column"] & date == data[i,"date"] & depth == depths[j]))
   bottom <- which(with(data, column == data[i,"column"] & date == data[i,"date"] & depth == depths[j+1]))
   
   F_calc_top <- data[center,"F_calc"]
   if(j==5) F_calc_bottom <- 0 else F_calc_bottom <- data[bottom,"F_calc"]
   if(length(F_calc_top)==0) F_calc_top <- NA
   if(length(F_calc_bottom)==0) F_calc_bottom <- NA
   
   data[center,"F_top_in"] <- ifelse(F_calc_top > 0, 0, abs(F_calc_top))
   data[center,"F_top_out"] <- ifelse(F_calc_top < 0, 0, abs(F_calc_top))
   data[center,"F_bottom_in"] <- ifelse(F_calc_bottom < 0, 0, abs(F_calc_bottom))
   data[center,"F_bottom_out"] <- ifelse(F_calc_bottom > 0, 0, abs(F_calc_bottom))

}

#' In n' out
data$F_out <- with(data, F_bottom_out + F_top_out)

#' Calculate top and bottom inputs for SP and d18O
for (i in complete) {
   
   # Create a `mask`, which is a logical vector selecting all current 
   j <- which(depths==data[i,"depth"]) # Get the current depth index
   mask <- with(data, column == data[i,"column"] & date == data[i,"date"])
   
   # Inputs from the bottom layer: if we're in the deepest increment, the inputs are all zero
   if(j==5) {
      data[mask & data$depth == depths[j],"SP_bottom"] <- 0
      data[mask & data$depth == depths[j],"d18O_bottom"] <- 0
   } else {
      x <- data[mask & data$depth == depths[j+1],"SP"]
      data[mask & data$depth == depths[j],"SP_bottom"] <- ifelse(length(x)==1,x,NA)
      x <- data[mask & data$depth == depths[j+1],"d18O"]
      data[mask & data$depth == depths[j],"d18O_bottom"] <- ifelse(length(x)==1,x,NA)
   }
   
   # Inputs from the top layer: if we're in the surface increment, the inputs are from the atmosphere
   if(j==1) {
      data[mask & data$depth == depths[j],"SP_top"] <- SPatm
      data[mask & data$depth == depths[j],"d18O_top"] <- d18Oatm
   } else {
      x <- data[mask & data$depth == depths[j-1],"SP"]
      data[mask & data$depth == depths[j],"SP_top"] <- ifelse(length(x)==1,x,NA)
      x <- data[mask & data$depth == depths[j-1],"d18O"]
      data[mask & data$depth == depths[j],"d18O_top"] <- ifelse(length(x)==1,x,NA)
   }
}

#' Write data to resources
write.csv(data, file = "data/data-calculated.csv", row.names = FALSE)
