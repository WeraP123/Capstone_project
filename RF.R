library(randomForest)
library(caret)
library(e1071)
library(dplyr)
#dataName = "ColonCA"
#fs=
df <- data.frame(SelectedExprs, Class)

trControl <- trainControl(method = "cv", number = 10, search ="grid")

set.seed(124)
trainIndex <- createDataPartition(df$Class, p=0.8, list = F, times =1)

Xtrain <- df[trainIndex, -c(11)]
ytrain <- df[trainIndex,c(11)]
Xtest <- df[-trainIndex, -c(11)]
ytest <- df[-trainIndex,c(11)]

Xtrain %>%
  dplyr::group_by(Class) %>%
  dplyr::summarise(count = n(),
                   percentage = round(length(Class)/nrow(dfTrain), 2))
Xtest %>%
  dplyr::group_by(Class) %>%
  dplyr::summarise(count = n(),
                   percentage = round(length(Class)/nrow(dfTest), 2))


bestRf <- e1071::best.randomForest(Xtrain, tunecontrol=tune.control(sampling = 'cross', cross=10), y=factor(ytrain))
prediction <- predict(bestRf, Xtest)
print(bestRf)
confusionMatrix(prediction, ytest)

rf_model <- train(Class~.,data=dfTrain,method = 'rf', metric='Accuracy', trControl=trControl, ntree=50)
print(rf_model)
print(bestRf)
prediction <-predict(rf_model, dfTest)
cm <- confusionMatrix(prediction, ytest)
save(cm, file=paste("Results/RF", dataName,"FS",fs, ".Rdata", sep=""))
