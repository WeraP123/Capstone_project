
# x is the matrix, data frame
wilcox_DEG <- function(x,y){ wilcox.test(x[y=="n"], x[y=="t"], 
                                            alternative="two.sided", paired=FALSE, 
                                            exact=FALSE)$p.value }
wilcox.pval <- apply(ExpressionData,1,wilcox_DEG,classColonCa)
sorted_wilcox.pval <- sort(wilcox.pval)
sorted_wilcox.pval[1:10]
column_names <- names(sorted_wilcox.pval)
column_names
tExpressionData <- t(ExpressionData)
SelColumnsExprs <- tExpressionData[,column_names]
