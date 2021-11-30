dt <-as.matrix(SelColumnsExprs, ClassLungFactor)
memory.limit()
library(caret)
set.seed(124)
trainCtrl <-trainControl(method='cv', number=10, savePredictions = TRUE)
rf.md.1 <- train(x = SelColumnsExprs, y = ClassLungFactor, method ='rf',trControl = trainCtrl, tuneLength =9, ntree=100)
#Error: protect(): protection stack overflow

library(randomForest)
library(caret)
#Performing RF with K-fold cross-validation
dtm <- data.frame(SelColumnsExprs, ClassLungFactor)
#NEW
trControl <- trainControl(method = "cv", number = 10, search ="grid")
set.seed(124)
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

rf_m.2 <- train(ClassLungFactor~.,data=dtmTrain,method = 'rf', metric='Accuracy', trControl=trControl, ntree=50)
print(rf_m.2)
prediction <-predict(rf_m.2, dtmTest)
confusionMatrix(prediction, dtmTest$ClassLungFactor)
##Accuracy : 1          
# Confusion Matrix and Statistics
# 
# Reference
# Prediction control tumor
# control       9     0
# tumor         0     9
# 
# Accuracy : 1          
# 95% CI : (0.8147, 1)
# No Information Rate : 0.5        
# P-Value [Acc > NIR] : 3.815e-06  
# 
# Kappa : 1          
# 
# Mcnemar's Test P-Value : NA         
#                                      
#             Sensitivity : 1.0        
#             Specificity : 1.0        
#          Pos Pred Value : 1.0        
#          Neg Pred Value : 1.0        
#              Prevalence : 0.5        
#          Detection Rate : 0.5        
#    Detection Prevalence : 0.5        
#       Balanced Accuracy : 1.0        
#                                      
#        'Positive' Class : control  