source("ModelSUBiNN.R")
library(caret)
#dataName=
#fs =
X <- scale(SelectedExprs)
y <- Class

X <- as.matrix(X)
colnames(X) <- 1:ncol(X)
levels(y) <- c(0, 1)
y <- as.numeric(y) - 1

idx <- sample(rep(1:10, length.out=nrow(X)))
for(i in 1:10) {
  Xtrain <- X[idx != i, ]
  ytrain <- y[idx != i]
  Xtest  <- X[idx == i, ]
  ytest  <- y[idx == i]}

result <- GetSUBiNNPredictions(Xtrain, Xtest, ytrain, TRUE)
cm <- confusionMatrix(data=factor(result[[1]]), reference = factor(ytest))
save(cm, file=paste("Results/SUBiNN", dataName, "FT", fs, ".Rdata", sep=""))