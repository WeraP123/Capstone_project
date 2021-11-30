library(propOverlap)
ExpressionDataLung <- t(ExpressionDataLun)
Score <- POS(ExpressionDataLung, CI.emprical(ExpressionDataLung, ClassLungFactor), ClassLungFactor)
Gene.mask <- GMask(ExpressionDataLung, CI.emprical(ExpressionDataLung, ClassLungFactor) ,ClassLungFactor)
Selection.k <- Sel.Features(ExpressionDataLung, ClassLungFactor, K=10, Verbose = T)
Selection.k$Features
SelFTNumbers <- as.vector(Selection.k[["Features"]])
SelFTNumbers <- sort(SelFTNumbers)
SelFTNumbers
tExpressionDataLung <- t(ExpressionDataLung)
library(dplyr)
SelColumnsExprs <- tExpressionDataLung[,c(256, 1547, 1815, 10462, 10699,10740,12404,12663,12810,14089)]
