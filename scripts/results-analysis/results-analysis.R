
data <- read.csv(file.path("results","totalResults.csv"))

df <- with(data, tapply(Nitrification_50., list(depth, variety), function(x) paste(signif(mean(x),3),"±",signif(sd(x),3))))
