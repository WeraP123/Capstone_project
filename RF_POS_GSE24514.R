library(randomForest)
#Performing RF with K-fold cross-validation
tExpressionData <- t(ExpressionData)
dt <- as.matrix(tExpressionData, Group = classColon)
library(caret)
set.seed(124)
trainCtrl <- trainControl(method = 'cv',number = 10, savePredictions = TRUE)
rf.m.1 <- train(Group~ .,data = dt, subselect = SelectedFeatures, method='rf', trControl = trainCtrl, tuneLength = 10, ntree=30)
#Error: protect(): protection stack overflow

