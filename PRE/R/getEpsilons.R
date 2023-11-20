#' @title Get the isotope end members
#' 
#' @param SPnit Site preference nitrification \insertCite{lewicka2017quantifying}{PRE}.
#' @param SPden Lewicka-Szceback for SPden.
#' @param eSPdiffusion Well et al. 2008 for eSPdiffusion.
#' @param eSPred Lewicka-Szceback for eSPred.
#' @param d18Onit Lewicka-Szceback for d18Onit.
#' @param d18Oden Lewicka-Szceback for d18Oden.
#' @param e18Odiffusion e18Odiffusion \insertCite{well2008isotope}{PRE}.
#' @param e18Ored Lewicka-Szceback e18Ored.
#' 
#' @returns A list of the eight isotope end members analogous to the function arguments.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
getEpsilons <- function(SPnit = 34.4,
                        SPden = -2.4,
                        eSPdiffusion = 1.55,
                        eSPred = -5.3,
                        d18Onit = 36.5,
                        d18Oden = 11.1,
                        e18Odiffusion = -7.79,
                        e18Ored = 3*eSPred) {
        list(SPnit = SPnit,
             SPden = SPden,
             eSPdiffusion = eSPdiffusion,
             eSPred = eSPred,
             d18Onit = d18Onit,
             d18Oden = d18Oden,
             e18Odiffusion = e18Odiffusion,
             e18Ored = e18Ored)
}