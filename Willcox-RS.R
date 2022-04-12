wilcox_DEG <- function(x,y){ wilcox.test(x[y=="0"], x[y=="1"], 
                                         alternative="two.sided", paired=FALSE, 
                                         exact=FALSE)$p.value }

wilcox.pval <- apply(ExpressionData,1,wilcox_DEG,Class)

ExpressionData <- t(ExpressionData)
sorted_wilcox.pval <- sort(wilcox.pval)
table(wilcox.pval<0.05)
formatC(sorted_wilcox.pval[1:10], format = "e", digits = 2)
column_names <- names(sorted_wilcox.pval[1:10])
tExpressionData <- t(ExpressionData)
SelectedExprsW <- tExpressionData[,column_names]


#adjusted p-values fdr
adj.wilcox.pval <- p.adjust(wilcox.pval, method="fdr")
sort.adj.pvalues <- sort(adj.wilcox.pval)
table(sort.adj.pvalues<0.05)
formatC(sort.adj.pvalues[1:70], format = "e", digits = 2)
adj.pnames <- names(sort.adj.pvalues[1:30])
SelectedExprs <- tExpressionData[,adj.pnames]




