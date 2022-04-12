
colnames(ExpressionData)<- c(1:ncol(ExpressionData))

colours = c("#56B4E9","#D55E00")

layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
par(oma = c(2, 2, 2, 1)) # Outer margins
table(Class)

# Density graph
par(mar=c(4,4,4,13))

d <- plot(density(ExpressionData), main="Distribution of expression values for all samples",lwd=2.5,xpd=F, ylab = "Density", xlab = "Expression values")


legend("right",inset = c(-0.4,0),legend = c("MSI colorectal tumor", "normal colonic mucosa"),xpd=T,bty="n",col=c("#56B4E9","#D55E00"),lwd = 3,lty=1)
# Boxplot

par(mar=c(4,4,4,4))
b <-boxplot(ExpressionData, boxwex=0.6, xlab="Samples",notch=F,outline = F,col=colours[Class],main="Distribution of expression values for each sample",xlab="Samples", ylab="Expression values")

#legend
mtext("GSE24514",side=3,line=0,outer=T)

