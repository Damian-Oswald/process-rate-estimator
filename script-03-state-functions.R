#' ---
#' title: Process Rate Estimator
#' author: Damian Oswald
#' date: 2023-10-02
#' ---


#' -----------------------------------------------------------------------------------------------------------
#' Prepare work space
#' -----------------------------------------------------------------------------------------------------------

source("script-01-data-preparation.R")
remove(denominator, F_calc_bottom, F_calc_top, i, j, mask, value, complete, concentration)

#' -----------------------------------------------------------------------------------------------------------
#' Run solver
#' -----------------------------------------------------------------------------------------------------------

load("resources/hyperparameters.RData")

getDerivative <- function(x, y, bandwidth = 1) {
   formula <- formula(paste("y", "x", sep = "~"))
   f <- np::npreg(formula, data = data, bws = bandwidth)
   predict(f)
}

with(data[data$column==1 & data$depth==7.5,],
     {
        getDerivative(x = as.numeric(date), y = N2O, bandwidth = 10)
     })

data[data$column==1 & data$depth==7.5,]

#' -----------------------------------------------------------------------------------------------------------
#' Run solver
#' -----------------------------------------------------------------------------------------------------------

library(BB)

help("multiStart", package = "BB")

attach(data[1,])

dN2Odt <- 1 # get this from the approximated function
SPtopin <- 1
SPbottomin <- 1
dSP_dt <- 1 # get this from the approximated function
d18Otopin <- 1
d18Obottomin <- 1
d18O_dt <- 1 # get this from the approximated function

eqset <- function(x = c(N2Onit = 0, N2Oden = 0, N2Ored = 0), e = unlist(getEpsilons())) {
   f <- numeric(length(x))
   f[1] <- F_top_in + F_bottom_in - F_out + x[1] + x[2] - x[3] - dN2O_dt
   f[2] <- (F_top_in*(SPtopin - e[3] - SP) + F_bottom_in*(SPbottomin - e[3] - SP) + x[1]*(e[1]-SP) + 
               x[2]*(e[2]-SP)-(e[3]*F_out + e[4]*x[3]))/N2O - dSP_dt
   f[3] <- (F_top_in*(d18Otopin - e[7] - d18O) + F_bottom_in*(d18Obottomin - e[7] - d18O) + 
               x[1]*(e[5]-d18O) + x[2]*(e[6]-d18O)-(e[7]*F_out + e[8]*x[3]))/N2O - d18O_dt
}

par <- multiStart(par = matrix(runif(1000*3, 0, 40), ncol = 3), fn = eqset, action = "solve", method = 2, 
                  control = list(tol = 5, noimp = 100, maxit = 1500),
                  details = FALSE,
                  quiet = TRUE)
boxplot(par$par)





