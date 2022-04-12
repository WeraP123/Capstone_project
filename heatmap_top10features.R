
library(ggplot2)
library(pheatmap)
library(viridis)


df <- data.frame(SelectedExprs, Class)
df2 <- data.frame(SelectedExprsW, Class)
df <- df[order(df$Class),]
x <- df[,-11]
x <- scale(x)
x <- as.matrix(x)


df2 = data.frame(SelectedExprsW,Class)
df2 <- df2[order(df2$Class),]
df2 <- scale(df2[,1:10])
w <- as.matrix(df2[,1:10])
a <- table(gset@phenoData@data[["source_name_ch1"]])
a <-table(Class)
a <-table(Class <- (gset@phenoData@data[["characteristics_ch1"]]))
a <- table(colonCA@phenoData@data[["class"]])
rownames(a)<-c("normal","tumor")
a[1]
b <- c(rep(" ",(a[1]/2)),rownames(a)[1],rep(" ",(a[1]/2)),rep(" ",(a[2]/2)-1),rownames(a)[2],rep("",a[2]/2))


b

#plotting heatmap showing the gene expression grouped by the class in the top 10 selected genes
par(mar=c(2,2,2,2))
virid
pheatmap(t(w),cluster_cols = F, cluster_rows = F,gaps_col = a[1],color = viridis(100),border_color = NA,labels_col = b,angle_col=0,main="Expression values of top 10 selected genes by WIl-RS")
pheatmap(t(x),cluster_cols = F, cluster_rows = F,gaps_col = a[1],color = viridis(100),border_color = NA,labels_col = b,angle_col=0,main="Expression values of top 10 selected genes by POS")


