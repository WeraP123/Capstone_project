library(Biobase)
library(propOverlap)
Score <- POS(ExpressionData, CI.emprical(ExpressionData,classColonCa), classColonCa)
summary(Score)
Selection.min <- Sel.Features(ExpressionData, classColonCa, K='10', Verbose = T)
SelectedFeatures <- as.vector(Selection.min[["Features"]])
SelectedFeatures <- sort(SelectedFeatures)
SelectedFeatures
tExpressionData <- t(ExpressionData)
library(dplyr)
SelColumnsExprs <- tExpressionData[,c(50,66,106,493,515,682,1522,1887,1920,1999)]

