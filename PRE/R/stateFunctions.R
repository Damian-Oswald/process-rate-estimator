#' @title State function set
#' 
#' @export
stateFunctions <- function(x, e = getEpsilons(), Ftopin, Fbottomin, Fout, dN2O_dt, SPtopin, SP, SPbottomin, N2O, dSP_dt, d18Otopin, d18Obottomin, d18O, d18O_dt) {
   f <- numeric(3)
   f[1] <- Ftopin + Fbottomin - Fout + x[1] + x[2] - x[3] - dN2O_dt
   f[2] <- (Ftopin*(SPtopin - e[3] - SP) + Fbottomin*(SPbottomin - e[3] - SP) + x[1]*(e[1]-SP) + 
               x[2]*(e[2]-SP)-(e[3]*Fout + e[4]*x[3]))/N2O - dSP_dt
   f[3] <- (Ftopin*(d18Otopin - e[7] - d18O) + Fbottomin*(d18Obottomin - e[7] - d18O) + 
               x[1]*(e[5]-d18O) + x[2]*(e[6]-d18O)-(e[7]*Fout + e[8]*x[3]))/N2O - d18O_dt
   f
}