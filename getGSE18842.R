library(GEOquery)
gset <- getGEO("GSE18842", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL570", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]
ExpressionDataLung <- exprs(gset)
boxplot(ExpressionDataLung, outline = FALSE)
summary(as.vector(ExpressionDataLung))
hist(ExpressionDataLung, breaks=80, probability = T)
ClassLung <- gset@phenoData@data[["sample type:ch1"]]
ClassLungFactor <- as.factor(ClassLung)
