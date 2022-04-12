library(GEOquery)
library(Biobase)
gset <- getGEO("GSE18842", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL570", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

ExpressionData <- exprs(gset)
Class <- factor(gset@phenoData@data[["sample type:ch1"]])
Class <- as.numeric(Class)-1
Class <- as.factor(Class)

table(Class)
summary(as.vector(ExpressionData))

skewness(ExpressionData)
kurtosis(ExpressionData)
