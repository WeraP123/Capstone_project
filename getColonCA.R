library(Biobase)
library(datasets)
library(colonCA)
data("colonCA")

Class <- colonCA@phenoData@data[["class"]]
ExpressionData <- exprs(colonCA)

summary(as.vector(ExpressionData))

ExpressionData <- log2(exprs(colonCA))
summary(as.vector(ExpressionData))
boxplot(ExpressionData, outline = F)
hist(ExpressionData, breaks=80, probability = T)
