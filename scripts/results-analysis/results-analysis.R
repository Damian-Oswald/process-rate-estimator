# ---
# title: Analysis of the PRE results
# ---

# Read data
data <- read.csv(file.path("scripts","run-process-rate-estimator","output","estimated-process-rates.csv"))

# create factor variable for variety
variety <- factor(c("Zinal", "Probus", "CH Claro", "Monte Calme 268", "Probus", "CH Claro", "Monte Calme 268", "CH Claro", "Probus", "Zinal", "Zinal", "Monte Calme 268"))

# Nitrification
df <- reshape2::melt(with(data, tapply(Nitrification, list(depth, column), mean)), varnames = c("depth", "column"))
df$variety <- variety[df$column]
lm(value ~ depth * as.factor(column), df) |> anova()
lm(value ~ variety, df) |> anova()

# Denitrification
df <- reshape2::melt(with(data, tapply(Denitrification, list(depth, column), mean)), varnames = c("depth", "column"))
df$variety <- variety[df$column]
lm(value ~ depth, df) |> anova()
lm(value ~ variety, df) |> anova()

# Reduction
df <- reshape2::melt(with(data, tapply(Reduction, list(depth, column), mean)), varnames = c("depth", "column"))
df$variety <- variety[df$column]
lm(value ~ depth * as.factor(column), df) |> anova()
