library(randomForest)
library(caret)
library(e1071)
trControl <- trainControl(method = "cv", number = 10, search ="grid")
trainIndex <- createDataPartition(dtm$ClassLungFactor, p=0.8, list = F, times =1)
dtmTrain <- dtm[trainIndex, ]
dtmTest <- dtm[-trainIndex, ]

dtmTrain %>%
  dplyr::group_by(ClassLungFactor) %>%
  dplyr::summarise(count = n(),
                   percentage = round(length(ClassLungFactor)/nrow(dtmTrain), 2))
dtmTest %>%
  dplyr::group_by(ClassLungFactor) %>%
  dplyr::summarise(count = n(),
                   percentage = round(length(ClassLungFactor)/nrow(dtmTest), 2))
set.seed(1234)

dtm <- data.frame(SelColumnsExprs,ClassLungFactor)
rf_m.2 <- train(ClassLungFactor~.,data=dtmTrain,method = 'rf', metric='Accuracy', trControl=trControl, ntree=50)
print(rf_m.2)
prediction <-predict(rf_m.2, dtmTest)
confusionMatrix(prediction, dtmTest$ClassLungFactor)
varImpPlot(rf_m.2)
