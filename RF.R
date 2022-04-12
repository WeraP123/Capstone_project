library(caret)
library(randomForest)

dataName = "GSE18842"
fs= "POS"
df = data.frame(t(ExpressionData), Class)
df = data.frame(SelectedExprs,Class)
df = data.frame(SelectedExprsW,Class)

df = data.frame(SelectedExprs,Class)
acc <- c()
vec <- c()

  for(j in 1:50){
    set.seed(31)
    trainIndex <- createDataPartition(df$Class, p=0.8, list = F, times =1)
    Xtrain <- df[trainIndex, -2001]
    ytrain <- df[trainIndex,2001]
    Xtest <- df[-trainIndex, -2001]
    ytest <- df[-trainIndex,2001]
    rf <- randomForest(Xtrain, ytrain,method="class")
    p <- predict(rf,Xtest)
    cm <- confusionMatrix(p,ytest)
    accuracy <- cm[["overall"]][["Accuracy"]]
    acc[j] <- accuracy
    vec <- c(mean(acc))
  }
vec
result_rf_wil[2,] <- vec

result_rf_wil <- data.frame("Acc"=0,"Kappa"=0)


save(result_subinn_pos, file=paste("Results/", dataName,"features",features, ".Rdata", sep=""))

trcontrol
vec
dataName ="gse5847"
ExpressionData <- t(ExpressionData)

save(Class,file=paste("Class", dataName,".Rdata",sep=""))
save(ExpressionData,file=paste("Exprs", dataName,".Rdata",sep=""))
df = data.frame(ExpressionData, Class)
save(df,file=paste("DF", dataName,".Rdata",sep=""))
