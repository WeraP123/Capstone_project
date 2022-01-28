library(Biobase)
library(propOverlap)
library(dplyr)

ExpressionData <- scale(ExpressionData)
Score <- POS(ExpressionData, CI.emprical(ExpressionData,Class), Class)
summary(Score)

Selection.min <- Sel.Features(ExpressionData, Class, K='10', Verbose = T)
SelectedFeatures <- as.vector(Selection.min[["Features"]])
SelectedFeatures <- sort(SelectedFeatures)
SelectedFeatures

tExpressionData <- t(ExpressionData)
SelectedExprs <- tExpressionData[,c(SelectedFeatures)]


