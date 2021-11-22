library(propOverlap)
tExpressionData2 <- t(expressionData2)
Score <- POS(tExpressionData2, CI.emprical(tExpressionData2, Tumor_Control), Tumor_Control)
Gene.mask <- GMask(tExpressionData2, CI.emprical(tExpressionData2, Tumor_Control) ,Tumor_Control)
Selection.k <- Sel.Features(tExpressionData2, Tumor_Control)
Selection.k$Features
