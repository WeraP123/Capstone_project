source("ModelSUBiNN.R")
library(caret)

dataName= "GSE24514"
fs = "WIL"
source("ModelSUBiNN.R")
library(caret)
df <- data.frame(SelectedExprs, Class)

x <- df[,-11]
x <- scale(x)
X <- as.matrix(x)

acc <- c()
acct <- c()

colnames(X) <- 1:ncol(X)
y <- as.numeric(Class)-1
for(j in 1:50){
  seed = j
  set.seed(j)
  idx <- sample(rep(1:5, length.out=nrow(X)))
  for(i in 1:5) {
    Xtrain <- X[idx != i, ]
    ytrain <- y[idx != i]
    Xtest  <- X[idx == i, ]
    ytest  <- y[idx == i]}

  result <- GetSUBiNNPredictions(Xtrain, Xtest, ytrain, TRUE)
  t <- table(factor(result[[1]], levels=min(ytest):max(ytest)), 
       
        factor(ytest, levels=min(ytest):max(ytest)))
  accuracyt <- (t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
  cm <- confusionMatrix(data=factor(result[[1]]), reference = factor(ytest))
  accuracy <- cm[["overall"]][["Accuracy"]]
  acc[j] <- accuracy
  acct[j] <- accuracyt
  vec <- c(mean(acct))
}

vec
result_subinn_pos[10,] <- vec
results <- vec
result_subinn_wil <- data.frame("Acc"=0)

save(results, file=paste("Results/SUBiNN", dataName, "FT", fs,"Seed", seed, ".Rdata", sep=""))
