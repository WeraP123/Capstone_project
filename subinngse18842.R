#!/usr/bin/Rscript
library(caret)
load("/home/wp19511/subinnR/Exprsgse24512.Rdata")
load("/home/wp19511/subinnR/Classgse24512.Rdata")

library(class)
library(glmnet)
GetBaseLearnerMatrix <- function(P) {
  learners <- cbind(rbind(combn(1:P, 1), 0), combn(1:P, 2))
  learners[2, 1:P] <- 1:P
  return(learners)
}

GenBaseLearnerTrainOutput <-  function(y, X, baseLearners, k){
  N <- nrow(X)
  P <- ncol(X)
  
  C <-  10
  folds <- sample(rep(1:C, length.out = nrow(X)))
  
  Z <-  matrix(NA, nrow = nrow(X), ncol = choose(P, 2) + P)
  
  for(c in 1:C){
    idx <- which(folds == c)
    
    for(p in 1:ncol(baseLearners)){
      if (k == 'opt'){
        tuneK <- e1071::tune.knn(X[-idx, as.character(baseLearners[, p]), drop=FALSE], factor(y[-idx]), k=1:10, tune.control = tune.control(sampling = 'cross', cross=10))
        k <- tuneK$best.parameters
      } else if (k == 'sqrt'){
        k <- round(sqrt(nrow(Xtrain)))
      } else {
        k <- as.numeric(k)
      }
      out <- class::knn(X[-idx, as.character(baseLearners[, p]), drop=FALSE], X[idx, as.character(baseLearners[, p]), drop=FALSE], y[-idx], k, prob = TRUE)
      Z[idx,p] <- ifelse(out==1, attr(out,"prob"), 1 - attr(out,"prob"))
    }
  }
  return(Z)
}

GenBaseLearnerTestOutput <- function(Xtrain, Xtest, ytrain, k, baseLearners, computeFor = c()) {
  P <- ncol(Xtrain)
  
  Z <- matrix(0, nrow(Xtest), (P + P*(P-1)/2))
  
  for (p in 1:ncol(baseLearners)){
    if (computeFor[p]){
      if (k == 'opt'){
        tuneK <- e1071::tune.knn(Xtrain[, as.character(baseLearners[, p]), drop=FALSE], factor(ytrain), k=1:10, tune.control = tune.control(sampling = 'cross', cross=10))
        k <- tuneK$best.parameters
      } else {
        k <- as.numeric(k)
      }
      out <- class::knn(Xtrain[, as.character(baseLearners[, p]), drop=FALSE], Xtest[, as.character(baseLearners[, p]), drop=FALSE], ytrain, k, prob = TRUE)
      Z[,p] = ifelse(out==1, attr(out,"prob"), 1 - attr(out,"prob"))
    }
  }
  return (Z)
}

GetLassoMetaLearnerFit <- function(ytrain, Ztrain){
  cv.fit <- cv.glmnet(Ztrain, as.numeric(ytrain), alpha=1, standardize=FALSE, family="gaussian", nfolds=10)
  fit <- glmnet(Ztrain, as.numeric(ytrain), alpha=1, standardize=FALSE, family="gaussian",
                lower.limits=0, intercept=FALSE, lambda=cv.fit$lambda.min)
  return(fit)
}

GetSUBiNNPredictions <- function(Xtrain, Xtest, ytrain, withPredictions = FALSE, k = 'sqrt') {
  baseLearners <- GetBaseLearnerMatrix(ncol(Xtrain))
  Ztrain <- GenBaseLearnerTrainOutput(ytrain, Xtrain, baseLearners, k)
  lassoFit <- GetLassoMetaLearnerFit(ytrain, Ztrain)
  coefs <-  matrix(lassoFit$beta, ncol=1)
  
  if (k == 'sqrt'){
    k <- round(sqrt(nrow(Xtrain)))
  }
  Ztest <- GenBaseLearnerTestOutput(Xtrain, Xtest, ytrain, k, baseLearners, coefs != 0)
  pred <- predict(lassoFit, newx = Ztest, type="class", s="lambda.min")
  predictions <- as.numeric(pred >= 0.5)
  ifelse(withPredictions,
         return(list(predictions, pred, coefs
         )),
         return(list(predictions, coefs)))
}

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