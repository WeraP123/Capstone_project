dt <-as.matrix(tEXpressionDataLung, Group = ClassLungFactor)
memory.limit()
library(caret)
set.seed(124)
trainCtrl <-trainControl(method='cv', number=10, savePredictions = TRUE)
rf.md.1 <- train(Group~.,data = dt,subset = SelFTNumbers, method ='rf',trControl = trainCtrl, tuneLength =10, ntree=50)
#Error: protect(): protection stack overflow