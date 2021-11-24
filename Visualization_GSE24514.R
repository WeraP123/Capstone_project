#Preprocessing
boxplot(ExpressionData, outline= FALSE)
summary(as.vector(ExpressionData))
#Values in range [0,16] log2 transformation already performed
hist(as.vector((ExpressionData)), breaks = 80, prob =T, xlab='Expression Levels', main = 'Histogram of Overall Expression Levels')
phenoData(gset)
#selecting sampleINFo
gset@phenoData@data
classColon <- gset@phenoData@data[["source_name_ch1"]]
classColon <- factor(gset@phenoData@data[["source_name_ch1"]])

