library(Biobase)
library(datasets)
library(colonCA)
data("colonCA")


Class <- colonCA@phenoData@data[["class"]]
Class <- as.numeric(Class)-1
Class <- as.factor(Class)
ExpressionData <- log2(exprs(colonCA))

hist(ExpressionData,probability = T)
table(Class)
summary(as.vector(ExpressionData))
sd(ExpressionData)

skewness(ExpressionData)
kurtosis(ExpressionData)
median(ExpressionData)
rownames(ExpressionData)[39] <- "HSAC07"
rownames(ExpressionData)[40] <- "HSAC071"
rownames(ExpressionData)[41] <- "HSAC072"
rownames(ExpressionData)[42] <- "HSAC073"
rownames(ExpressionData)[50] <- "UMGAP"
rownames(ExpressionData)[51] <- "UMGAP1"
rownames(ExpressionData)[52] <- "UMGAP2"
rownames(ExpressionData)[53] <- "UMGAP3"
rownames(ExpressionData)[260] <- "i"
rownames(ExpressionData)[261] <- "i1"
rownames(ExpressionData)[262] <- "i2"
rownames(ExpressionData)[263] <- "i3"
