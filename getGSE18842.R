library(GEOquery)
gset <- getGEO("GSE18842", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL570", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]
expressionData2 <- exprs(gset)
boxplot(expressionData2, outline = FALSE)
summary(as.vector(expressionData2))
hist(expressionData2, breaks=80, probability = T)
Tumor_Control <- gset@phenoData@data[["sample type:ch1"]]
Tumor_Control <- as.factor(Tumor_Control)