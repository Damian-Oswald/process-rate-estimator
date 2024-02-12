
# attach packages to search path
library(PRE)

# determine a range of bulk density to be searched
BD <- getParameters()$BD + seq(-0.10, 0.10, 0.05)

# construct a function to return the process estimates for a give bulk density `x`
f <- function(x) {
    parameters <- getParameters(BD = x)
    data <- subset(measurements, column==1) |>
        getN2ON(parameters = parameters) |>
        getMissing() |>
        calculateFluxes(parameters = parameters, verbose = FALSE)
    result <- t(sapply(getParameters()$depth, function(d) longPRE(data, 1, d, n = 50, verbose = FALSE)$processes))
    return(data.frame(BD = x, depth = getParameters()$depth, result))
}

# apply said function to all bulk densities
data <- data.frame(BD = numeric(), depth = numeric(), Nitrification = numeric(), Denitrification = numeric(), Reduction = numeric())
for (i in BD) {
    data <- rbind(data, f(i))
    cat(i,"\n")
}

# visualize the results
svg(file.path("scripts","sensitivity-analysis","output","bulk-density-sensitiviy.svg"), width = 8, height = 5)
colors <- viridis::cividis(3, begin = 0.3, end = 0.8)
par(mar = c(4,4,0,0)+0.1)
plot(Nitrification ~ BD, data, ylim = range(data[,3:5]), type = "o", col = "transparent", las = 1,
     ylab = "Process rate", xlab = "Bulk density")
grid(col = 1)
for (d in getParameters()$depth) {
    lines(Nitrification ~ BD, subset(data, depth==d), type = "o", lwd = 2, pch = 16, col = colors[1])
    lines(Denitrification ~ BD, subset(data, depth==d), type = "o", lwd = 2, pch = 16, col = colors[2])
    lines(Reduction ~ BD, subset(data, depth==d), type = "o", lwd = 2, pch = 16,  col = colors[3])
}
legend("topleft", legend = colnames(data[,3:5]), col = colors, lwd = 2, pch = 16, inset = c(0.01,0.01), bg = "white")
dev.off()

# save table
write.csv(x = data,
          file = file.path("scripts","sensitivity-analysis","output","bulk-density-sensitiviy.csv"),
          row.names = FALSE)


