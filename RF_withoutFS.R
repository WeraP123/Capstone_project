library(randomForest)
#Performing RF with K-fold cross-validation
dt <- data.frame(t(ExpressionData), Group = as.factor(sampleInfo$group))
library(caret)
set.seed(124)
trainCtrl <- trainControl(method = 'cv',number = 10, savePredictions = TRUE)
rf.m.1 <- train(Group~ .,data = dt, method='rf', trControl = trainCtrl, tuneLength = 10, ntree=100)
#Error: protect(): protection stack overflow
dft <- data.matrix(dt)
rf.m.1 <- train(Group~ .,data = dft, method='rf', trControl = trainCtrl, tuneLength = 10, ntree=100)
