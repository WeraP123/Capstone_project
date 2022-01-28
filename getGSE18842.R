library(GEOquery)
gset <- getGEO("GSE18842", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL570", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

ExpressionData <- exprs(gset)
Class <- factor(gset@phenoData@data[["sample type:ch1"]])

boxplot(ExpressionData, outline = FALSE)
summary(as.vector(ExpressionData))
hist(ExpressionData, breaks=80, probability = T)


