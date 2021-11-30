library(randomForest)
library(caret)
#Performing RF with K-fold cross-validation
dtm <- data.frame(SelColumnsExprs, classColonCa)
#NEW
trControl <- trainControl(method = "cv", number = 10, search ="grid")
set.seed(124)
trainIndex <- createDataPartition(dtm$classColonCa, p=0.8, list = F, times =1)
dtmTrain <- dtm[trainIndex, ]
dtmTest <- dtm[-trainIndex, ]

dtmTrain %>%
  dplyr::group_by(classColonCa) %>%
  dplyr::summarise(count = n(),
                   percentage = round(length(classColonCa)/nrow(dtmTrain), 2))
dtmTest %>%
  dplyr::group_by(classColonCa) %>%
  dplyr::summarise(count = n(),
                   percentage = round(length(classColonCa)/nrow(dtmTest), 2))
set.seed(1234)

rf_m.2 <- train(classColonCa~.,data=dtmTrain,method = 'rf', metric='Accuracy', trControl=trControl, ntree=50)
print(rf_m.2)
prediction <-predict(rf_m.2, dtmTest)
confusionMatrix(prediction, dtmTest$classColon)

