
data <- read.csv(file.path("scripts","run-process-rate-estimator","output","estimated-process-rates.csv"))

df <- with(data, tapply(Nitrification, list(depth, variety), function(x) paste(signif(mean(x),3),"±",signif(sd(x),3))))

df <- reshape2::melt(with(data, tapply(Nitrification, list(depth, column), mean)), varnames = c("depth", "column"))
lm(value ~ depth * as.factor(column), df) |> anova()

df <- reshape2::melt(with(data, tapply(Denitrification, list(depth, column), mean)), varnames = c("depth", "column"))
lm(value ~ depth, df) |> anova()

df <- reshape2::melt(with(data, tapply(Reduction, list(depth, column), mean)), varnames = c("depth", "column"))
lm(value ~ depth * as.factor(column), df) |> anova()
