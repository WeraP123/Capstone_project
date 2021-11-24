library(Biobase)
library(propOverlap)
Score <- POS(ExpressionData, CI.emprical(ExpressionData,classColon), classColon)
summary(Score)
Selection.min <- Sel.Features(ExpressionData, classColon, K='20', Verbose = T)
SelectedFeatures <- as.vector(Selection.min[["Features"]])
SelectedFeatures <- sort(SelectedFeatures)
SelectedFeatures
