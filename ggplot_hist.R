library(ggplot2)
library(tidyr)


genes <- data.frame(t(ExpressionData),Class)
genes$sample <- 1:95

dfgg <- genes %>% gather(gene,value,-Class,-sample)
head(dfgg)
dataname="GSE5847"
colours = c("#56B4E9","#D55E00")
# histogram by groups
ggplot(dfgg, aes(x=value,fill=Class))+geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

# histogram with density
g <- ggplot(dfgg,aes(x = value))+geom_histogram(aes(y = after_stat(density)), bins = 60,fill="lightskyblue")
g + labs(title="Distibution of gene expression values", subtitle = dataname)+xlab(label ="log2 transformed gene expression values")

# density plot
p <- ggplot(dfgg, aes(ExpressionData))+geom_density(alpha=0.4,size=1)+
  labs(subtitle="Gene expresion values distribution")+theme_bw()

