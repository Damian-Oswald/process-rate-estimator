#' @title Calculate the volumetric and area N2O-N
#' 
#' @description This function calculates the volumetric and the area N2O-N from the soil N2O concentration (as stored in the variable `N2O` of [`PRE::measurements`]).
#' 
#' @details In order to calculate the volume N2O-N, the following equation is used:
#' \deqn{\text N_2 \text {O-N}_\text{volume} =  \frac{28[\text{N}_2\text O]}{\text {R} \cdot \text {T}}}
#' Where \eqn{\text R} is the gas constant and \eqn{\text T} is the temperature.
#' 
#' @param data The original data we wish to interpolate. Needs to have the same variables as [measurements].
#' 
#' @export
getN2ON <- function(data = PRE::measurements, parameters = getParameters()) {
    
    # attach the parameters to function environment
    list2env(parameters, environment())
    
    # calculate the volumetric and area N2O-N
    data[,"N2ONvolume"] <- with(data, N2O * 1/(R*temperature)*28)
    data[,"N2ONarea"] <- with(data, N2ONvolume * increment/100 * (theta_t - moisture) * 10000/1000)
    
    return(data)
}
