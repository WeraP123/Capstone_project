wilcox_DEG <- function(x,y){ wilcox.test(x[y=="control"], x[y=="tumor"], 
                                         alternative="two.sided", paired=FALSE, 
                                         exact=FALSE)$p.value }
wilcox.pval <- apply(ExpressionDataLun,1,wilcox_DEG,ClassLung)
sorted_wilcox.pval <- sort(wilcox.pval)
sorted_wilcox.pval[1:10]
column_names <- names(sorted_wilcox.pval[1:10])
column_names
tExpressionData <- t(ExpressionDataLun)
SelColumnsExprs <- tExpressionData[,column_names]
