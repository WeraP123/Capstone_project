library(ggplot2)
library(dplyr)
library(grid)
df <- data.frame(ExpressionData["221652_s_at",], Class)
df

rownames(df) <- 1:49

df %>%
  ggplot(aes(x=ExpressionData..221652_s_at...., y=Class,colour=Class,group=Class))+
  geom_point(size=2.5)+theme_bw()+
  geom_vline(xintercept = c(4.5735,6.0895), linetype=5,size=1)+
  annotation_custom(grob)
table(df$Class)

summary(df[1:34,"ExpressionData..221652_s_at...."])
summary(df[35:49,"ExpressionData..221652_s_at...."])


grob <- grobTree(textGrob("←Overlap region→", x=0.12,  y=0.95, hjust=0,
                          gp=gpar(col="black", fontsize=15, fontface="italic")))














test <- wilcox.test(df$ExpressionData..221652_s_at....~df$Class)
tExpressionData <- t(ExpressionData)
SelectedExprs <- tExpressionData[,column_names]
a.0 <- ExpressionData["221652_s_at",1:34]
a.1 <- ExpressionData["221652_s_at",35:49]
plot(density(a.0))
plot(density(a.1))
plot(density(a.0 <- ExpressionData["221652_s_at",]))
v <- c(14,15,16,18,20:49)
length(vv)
sum(vv)
vv <- c(1:13,17,19)
boxplot(SelectedExprs)
table(wilcox.pval<0.05)
34*15+(17*35)-1098
sort(a.0)
sort(a.1)