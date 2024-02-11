
# attach packages to search path
library(PRE)

# determine a range of bulk density to be searched
BD <- getParameters()$BD + seq(-0.15, 0.15, 0.05)

# construct a function to return the process estimates for a give bulk density `x`
f <- function(x) {
    parameters <- getParameters(BD = x)
    data <- subset(measurements, column==1) |>
        getN2ON(parameters = parameters) |>
        getMissing() |>
        calculateFluxes(parameters = parameters)
    result <- t(sapply(getParameters()$depth, function(d) longPRE(data, 1, d, n = 3)$processes))
    return(data.frame(depth = getParameters()$depth, result))
}

# apply said function to all bulk densities
result <- t(sapply(BD, f))

# visualize the results
svg(file.path("scripts","sensitivity-analysis","output","bulk-density-sensitiviy.svg"), width = 8, height = 5)
colors <- viridis::cividis(3, begin = 0.3, end = 0.8)
par(mar = c(4,4,0,0)+0.1)
with(as.data.frame(result), {
    plot(Nitrification ~ BD, ylim = range(result), type = "o", col = "transparent", las = 1,
         ylab = "Process rate", xlab = "Bulk density")
    grid(col = 1)
    lines(Nitrification ~ BD, type = "o", lwd = 2, pch = 16, col = colors[1])
    lines(Denitrification ~ BD, type = "o", lwd = 2, pch = 16,  col = colors[2])
    lines(Reduction ~ BD, type = "o", lwd = 2, pch = 16, col = colors[3])
    legend("topleft", legend = colnames(result), col = colors, lwd = 2, pch = 16, inset = c(0.01,0.01), bg = "white")
})
dev.off()

# save table
write.csv(x = data.frame(BD = BD, result),
          file = file.path("scripts","sensitivity-analysis","output","bulk-density-sensitiviy.csv"),
          row.names = FALSE)

m1 <- lm(Nitrification ~ BD, data.frame(result))
m2 <- lm(Denitrification ~ BD, data.frame(result))
m3 <- lm(Reduction ~ BD, data.frame(result))
stargazer::stargazer(m1,m2,m3,type="text")

