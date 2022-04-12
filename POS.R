library(Biobase)
library(propOverlap)

Score <- POS(ExpressionData, CI.emprical(ExpressionData,Class), Class)
summary(Score)

Selection.min <- Sel.Features(ExpressionData, Class, K='10', Verbose = F)
Selection.min$Features
SelectedFeatures <- as.vector(Selection.min[["Features"]])
tExpressionData <- t(ExpressionData)
SelectedExprs <- tExpressionData[,c(SelectedFeatures)]


summary(as.vector(SelectedExprs))
