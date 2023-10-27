#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-16
#' ---

#' -----------------------------------------------------------------------------------------------------------
#' Attach packages to search path
#' -----------------------------------------------------------------------------------------------------------

library(jsonlite)

#' -----------------------------------------------------------------------------------------------------------
#' Set Model Parameters
#' -----------------------------------------------------------------------------------------------------------

#' Bulk density, average across all depths and columns.
BD <- 1.686 # Bulk density
PD <- 2.65 # Particle density?
theta_t <- 1- BD/PD # = porosity, kg minerals/m3 soil * m3 minerals/kg minerals = 1 - m3 minerals/m3 soil = m3 pore/m3 soil
temperature <- 298
D_fw <- (5.07e-6) * exp(-2371/temperature)
D_fa <- 0.1436e-4 * (temperature/273.15)^1.81
H <- (8.5470e6 * exp(-2284/temperature)) / (8.3145*temperature)
rho <- 1.26e6
N2Oatm <- 0.2496 # ppmv; atmospheric N2O concentration
R <- 0.082 # Gas constant

#' Write model parameters to a list
parameters <- list(BD = BD, # t/m3
                   theta_t = theta_t,
                   temperature = temperature,
                   D_fw = D_fw, 
                   D_fa = D_fa,
                   H = H,
                   rho = rho,
                   N2Oatm = N2Oatm,
                   R = R,
                   depths = c(7.5, 30, 60, 90, 120))

#' Write a json file containing the parameters
jsonlite::write_json("resources/parameters.json", x = parameters, digits = 12, pretty = TRUE)
