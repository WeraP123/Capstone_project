library(GEOquery)
library(Biobase)
gset <- getGEO("GSE24514", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL96", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

ExpressionData <- exprs(gset)
Class <- factor(gset@phenoData@data[["source_name_ch1"]])

