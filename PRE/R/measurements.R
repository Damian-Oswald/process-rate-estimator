#' @title Soil N₂O flux measurements
#' 
#' @description This data was measured in Zürich for the Process Rate Estimator.
#' 
#' @usage 
#' data(measurements, package = "PRE")
#' 
#' @format ## `measurements`
#' A data frame with 9558 rows and  14 columns:
#' \describe{
#'   \item{`date`}{Date of the measurement \[`YYYY-MM-DD`\].}
#'   \item{`column`}{Experimental unit.}
#'   \item{`depth`}{Measurement depth \[cm\]. Either 7.5, 30, 60, 90, or 120 cm.}
#'   \item{`increment`}{Height of one conceptual soil layer (borders between the different depth layers).}
#'   \item{`variety`}{The wheat variety as a factor. Either `"CH Claro"`, `"Monte Calme 268"`, `"Probus"`, or `"Zinal"`.}
#'   \item{`moisture`}{The average moisture over a day.}
#'   \item{`N2O`}{The corrected N₂O concentration.}
#'   \item{`CO2`}{The corrected CO₂ concentration.}
#'   \item{`SP`}{The site preference.}
#'   \item{`d18O`}{The δ¹⁸O concentration.}
#'   \item{`d15Nbulk`}{The bulk δ¹⁵N concentration.}
#'   \item{`d15Nalpha`}{The δ¹⁵N α concentration.}
#'   \item{`d15Nbeta`}{The δ¹⁵N β concentration.}
#'   ...
#' }
#' 
#' @source <https://writethesourceurlorthepaper.com>
"measurements"