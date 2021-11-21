#Preprocessing
boxplot(ExpressionData, outline= FALSE)
summary(as.vector(ExpressionData))
#Values in range [0,16] log2 transformation already performed
hist(as.vector((ExpressionData)), breaks = 80, prob =T, xlab='Expression Levels', main = 'Histogram of Overall Expression Levels')
phenoData(gset)
#selecting sampleINFo
library(dplyr)
pD <- phenoData(gset)
varMetadata(pD)
gset@phenoData@data
pD <- gset@phenoData@data
sampleInfo <- select(pD, title, source_name_ch1)
sampleInfo <-rename(sampleInfo, patient=title, group=source_name_ch1)
#Group as factor
gset@phenoData@data[["source_name_ch1"]] <- factor(gset@phenoData@data[["source_name_ch1"]])

