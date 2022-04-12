
#!/usr/bin/Rscript
# File: subinncolonca.R
library(caret)

load("/home/wp19511/subinnR/ModelSUBiNN.R")
load("/home/wp19511/subinnR/Exprscolonca.Rdata")
load("/home/wp19511/subinnR/Classcolonca.Rdata")

X <- scale(ExpressionData)
X <- as.matrix(X)
colnames(X) <- 1:ncol(X)
y <- as.numeric(Class)-1

acc <- c()
vec <-c()
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
  cm <- confusionMatrix(data=factor(result[[1]]), reference = factor(ytest))
  accuracy <- cm[["overall"]][["Accuracy"]]
  acc[j] <- accuracy
  vec <- c(mean(acc))
}
print(vec)