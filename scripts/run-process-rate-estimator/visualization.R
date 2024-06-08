# ---
# title: Process Rate Estimator
# author: Damian Oswald
# date: 2024-06-08
# description: This script produces the visualizations of the process rates over time
# ---

# PREPARATION
# ===========

# attach the package to the search path
library(PRE)
library(magrittr)

# read results data frame of the estimated process rates
data <- read.csv(file.path("scripts","run-process-rate-estimator","output","estimated-process-rates.csv"), row.names = 1)

# convert data frame variable types
data$date %<>% as.Date()
data$column %<>% as.ordered()
data$depth %<>% as.ordered()

# define functions for plotting
varplot <- function(x, xs, t, ...) {
    plot(x ~ t, pch = 16, las = 1, xlab = "", xaxs = "i", ...)
    lines(xs ~ t, lwd = 2, col = "#fc5d5e")
    grid(col = 1)
}
processplot <- function(x, x_2.5, x_25, x_50, x_75, x_97.5, t, ...) {
    plot(x ~ t, type = "l", pch = 16, las = 1, xlab = "", xaxs = "i", ...)
    polygon(y = c(x_25, rev(x_75)),
            x = c(t, rev(t)),
            col = "#fc5d5e", border = FALSE)
    polygon(y = c(x_2.5, rev(x_97.5)),
            x = c(t, rev(t)),
            col = adjustcolor("#fc5d5e", alpha.f = 0.5),
            border = FALSE)
    lines(x_50 ~ t, lwd = 2)
    grid(col = 1)
}

# Run for-loop on all
for (c in levels(data$column)) {
    
    for (d in levels(data$depth)) {
        
        # Write all results as an SVG file
        svg(file.path("scripts","run-process-rate-estimator","output",sprintf("visualized-process-rates-C%s-D%s.svg", c, d)),
            width = 12, height = 8)
        
        # create a subset of the data
        df <- subset(data, subset = column==c & depth==d)
        
        # `try()` prevents crash when input data is missing
        par(mar = c(4,4,0,0)+0.5)
        layout(mat = matrix(c(1,2,3,4,5,6,4,5,6,4,5,6), nrow = 3, byrow = FALSE))
        
        varplot(df$N2O_measurement, df$N2O, df$date, ylim = c(0,10),
                ylab = expression("N"[2]*"O - N"[area]))
        varplot(df$SP_measurement, df$SP, df$date, ylim = c(-10,25),
                ylab = "Site Preference (SP)")
        varplot(df$d18O_measurement, df$d18O, df$date, ylim = c(20,55),
                ylab = expression("δ"^18*"O"))
        
        processplot(df$Nitrification, df$Nitrification_2.5., df$Nitrification_25., df$Nitrification_50.,
                    df$Nitrification_75., df$Nitrification_97.5.,
                    df$date, ylim = c(-10,60),
                    ylab = "Nitrification")
        processplot(df$Denitrification, df$Denitrification_2.5., df$Denitrification_25., df$Denitrification_50.,
                    df$Denitrification_75., df$Denitrification_97.5.,
                    df$date, ylim = c(-10,60),
                    ylab = "Denitrification")
        processplot(df$Reduction, df$Reduction_2.5., df$Reduction_25., df$Reduction_50.,
                    df$Reduction_75., df$Reduction_97.5.,
                    df$date, ylim = c(-10,60),
                    ylab = "Reduction")
        
        # close the writing process to SVG again
        dev.off()
    }
}
