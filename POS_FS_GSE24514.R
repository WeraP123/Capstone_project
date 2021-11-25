library(Biobase)
library(propOverlap)
Score <- POS(ExpressionData, CI.emprical(ExpressionData,classColon), classColon)
summary(Score)
Selection.min <- Sel.Features(ExpressionData, classColon, K='20', Verbose = T)
SelectedFeatures <- as.vector(Selection.min[["Features"]])
SelectedFeatures <- sort(SelectedFeatures)
SelectedFeatures
tExpressionData <- t(ExpressionData)
library(dplyr)
SelColumnsExprs <- tExpressionData[,c(759,779,2387,2530,2783,3812,4168,5477,5634,5734,5735,6529,9175,9455, 12426, 17872, 18262, 19390, 19511, 20198)]
SelColumnsDt <- dt[,c(759,779,2387,2530,2783,3812,4168,5477,5634,5734,5735,6529,9175,9455, 12426, 17872, 18262, 19390, 19511, 20198)]
