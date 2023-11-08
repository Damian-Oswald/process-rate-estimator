#' @title Get the PRE parameters.
#' 
#' @description This function returns the initial parameters necessary to run the Process Rate Estimator.
#' No function arguments have to be provided, however, if any default values are changed, the set of PRE parameters are calculated accordingly.
#' 
#' The function returns a list of parameters that are used to run the Process Rate Estimator (PRE).
#' The function return values are the same as the function arguments, but computed dependently.
#' 
#' @param BD Bulk density.
#' @param PD Particle density.
#' @param theta_t Total soil porosity.
#' @param temperature Soil temperature \[K\].
#' @param D_fw Diffusivity of N₂O in water.
#' @param D_fa Diffusivity of N₂O in air.
#' @param H Dimensionless Henry's constant \[-\].
#' @param rho Gas density of N₂O.
#' @param N2Oatm Atmospheric N₂O concentration \[ppmv\].
#' @param SPatm Atmospheric SP concentration.
#' @param d18Oatm Atmospheric d18O concentration.
#' @param R Gas constant \[L⋅atm⋅K⁻¹⋅mol⁻¹\].
#' @param depths A numeric vector containing the measurement depths \[cm\].
#' 
#' @export
getParameters <- function(BD = 1.686,
                          PD = 2.65,
                          theta_t = 1 - BD/PD,
                          temperature = 298,
                          rho = 1.26e6,
                          R = 0.082,
                          N2Oatm = 0.2496,
                          SPatm = 15.1,
                          d18Oatm = 49.6,
                          D_fw = 5.07e-6 * exp(-2371/temperature),
                          D_fa = 0.1436e-4 * (temperature/273.15)^1.81,
                          H = (8.5470e6 * exp(-2284/temperature)) / (8.3145*temperature),
                          depths = c(7.5, 30, 60, 90, 120)) {
   list(BD = BD,
        theta_t = theta_t,
        temperature = temperature,
        D_fw = D_fw, 
        D_fa = D_fa,
        H = H,
        rho = rho,
        N2Oatm = N2Oatm,
        SPatm = SPatm,
        d18Oatm = d18Oatm,
        R = R,
        depths = depths)
}
