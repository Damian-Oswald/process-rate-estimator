#' @title Calculate fluxes from measurement data
#' 
#' @description
#' This function calculates fluxes from measurement data (e.g. `PRE::measurements`).
#' 
#' @param data Measurement data from which one wants to calculate fluxes.
#' Needs to include variables `date`, `column`, `depth`, `increment`, `moisture`, `N2O`, `CO2`, `SP`, and `d18O`. For further details, read the description of the [`measurements`] data.
#' @param parameters A set of parameters as returned by the function [`getParameters`]. These will be used in calculating the returned flux data.
#' 
#' @export
calculateFluxes <- function(data = PRE::measurements, parameters = PRE::getParameters()) {
    
    # attach the parameters to function environment
    list2env(parameters, environment())
    
    # save index vector of complete observations
    complete <- data |> apply(2, is.na) |> apply(1, any) |> PRE::invert() |> which()
    
    # loop through every complete observation in the data frame
    for (i in complete) {
        
        # attach the related indices and masks of the current index i to the search path
        list2env(PRE::getIndices(data, i, parameters), environment())
        
        # calculate the mean of the current depth moisture and the one above (if it is the top layer, that's the mean of one observation)
        data[i,"theta_w"] <- mean(c(data[i, "moisture"], data[i_top, "moisture"]), na.rm = TRUE)
        
        # calculate air-filled pore space
        data[i,"theta_a"] <- with(data[i,], 1 - theta_w/theta_t)
        
        # calculate Ds
        data[i,"Ds"] <- with(data[i,], ((theta_w^(10/3)*D_fw)/H+theta_a^(10/3)*D_fa)*theta_t^-2)
        
        # calculate the N2O concentration gradient dC/cZ
        dC <- (data[i,"N2O"] - ifelse(j==1, N2Oatm, data[i_top,"N2O"]))/1000000
        if(length(dC)==0) dC <- NA
        dZ <- (diff(c(0,depths))/100)[j] # dZ is the distance from one measurement depth to the next in meters
        data[i,"dCdZ"] <- dC/dZ
        
        # calculate F_calc, considering flux is upward (originally in mg N2O-N/m2/s, converted to g N/ha/day)
        data[i,"F"] <- with(data[i,], dCdZ * Ds * rho * 10000 * 3600 * 24/1000)
        
    }
    
    # start a new loop, to make sure all `F` have been calculated already
    for (i in complete) {
        
        # attach the related indices and masks of the current index i to the search path
        list2env(PRE::getIndices(data, i, parameters), environment())
        
        # save F_calc for top and bottom
        F_top <- data[i,"F"]
        if(j==5) F_bottom <- 0 else F_bottom <- data[i_bottom,"F"]
        
        # avoid empty F_top or F_bottom
        if(length(F_top)==0) F_top <- NA
        if(length(F_bottom)==0) F_bottom <- NA
        
        # calculate inputs and outputs
        data[i,"F_top_in"] <- ifelse(F_top > 0, 0, abs(F_top))
        data[i,"F_top_out"] <- ifelse(F_top < 0, 0, abs(F_top))
        data[i,"F_bottom_in"] <- ifelse(F_bottom < 0, 0, abs(F_bottom))
        data[i,"F_bottom_out"] <- ifelse(F_bottom > 0, 0, abs(F_bottom))
        
        # calculate F_out
        data[i,"F_out"] <- with(data[i,], F_bottom_out + F_top_out)
        
        # save SP and d18O inputs from the bottom layer (if we're in the deepest increment, the inputs are all zero)
        if(j==5) {
            data[i, "SP_bottom"] <- 0
            data[i,"d18O_bottom"] <- 0
        } else {
            data[i, "SP_bottom"] <- ifelse(length(data[i_bottom, "SP"])==1, data[i_bottom, "SP"], NA)
            data[i,"d18O_bottom"] <- ifelse(length(data[i_bottom, "d18O"])==1, data[i_bottom, "d18O"], NA)
        }
        
        # save SP and d18O inputs from the top layer (if we're in the surface increment, the inputs are from the atmosphere)
        if(j==1) {
            data[i, "SP_top"] <- SPatm
            data[i, "d18O_top"] <- d18Oatm
        } else {
            data[i, "SP_top"] <- ifelse(length(data[i_top, "SP"])==1, data[i_top, "SP"], NA)
            data[i, "d18O_top"] <- ifelse(length(data[i_top, "d18O"])==1, data[i_top, "d18O"], NA)
        }
        
    }
    
    return(data)
}
