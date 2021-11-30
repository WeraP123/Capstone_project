library(Biobase)
library(propOverlap)
Score <- POS(ExpressionData, CI.emprical(ExpressionData,classColon), classColon)
summary(Score)
Selection.min <- Sel.Features(ExpressionData, classColon, K='10', Verbose = T)
SelectedFeatures <- as.vector(Selection.min[["Features"]])
SelectedFeatures <- sort(SelectedFeatures)
SelectedFeatures
tExpressionDataColon <- t(ExpressionData)
library(dplyr)
SelColumnsExprs <- tExpressionDataColon[,c(759,2387,2530,2783,3812,4168,5477,5634,9175, 12426)]

