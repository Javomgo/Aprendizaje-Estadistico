# Below we will 
# - use CV to choose tuning parameters on training data for random forest and boosting.
# - use validation to select winner (random forest or boosting?)
# - use CV on training and validation sets to train the final model which is evaluated using test.

# This options is valid in cases where the sample size is large. For smaller samples it 
# is recommended to using nested CV. (As found in ....)

dataWN <- read.table("drug_consumption001.txt", header=TRUE)
dfWN1 <- data.frame(dataWN)
summary(dfWN1)
#install.packages("caret")
require("caret")
require("randomForest")
require("gbm")

set.seed(181220181)

# Make 60% to train, 20% to validate, and 20% to test (your prefered model)

inTraining <- createDataPartition(dfWN1$coc, p=0.6, list=FALSE)
training.set <- dfWN1[inTraining,]
Totalvalidation.set <- dfWN1[-inTraining,]
# This will create another partition of the 40% of the data, so 20%-testing and 20%-validation
inValidation <- createDataPartition(Totalvalidation.set$coc, p=0.5, list=FALSE)
testing.set <- Totalvalidation.set[inValidation,]
validation.set <- Totalvalidation.set[-inValidation,]

dataset <- training.set
validation <- validation.set
test <- testing.set

# The final model will actually use all data, except test
total <- rbind(dataset, validation)

# We will use CARET

# I reduce the grid to save time here. 

fitControl <- trainControl(method = 'cv', number = 5, summaryFunction=defaultSummary)

getModelInfo()$gbm$parameters
gbmGrid <-  expand.grid(interaction.depth = c(1,4,7,10),
                        n.trees = c(500,1000,2000),
                        shrinkage = c(.005, .02,.05),
                        n.minobsinnode = 10)
gbmGrid

getModelInfo()$rf$parameters
#mtry max:
ncol(dfWN1)-1

rfGrid <-  expand.grid(mtry = c(1,2,3,4,6,9,12))
rfGrid

### Gradient boosting machine algorithm. ###
fit.gbm <- train(coc~., data=dataset, method = 'gbm', trControl=fitControl, tuneGrid=gbmGrid, metric='Accuracy', distribution='multinomial')
fit.gbm
plot(fit.gbm)
fit.gbm$bestTune

res_gbm <- fit.gbm$results
acc_gbm <- subset(res_gbm[5])
# CV con mejor "tune"
max(acc_gbm)
# 0.6165743

boost.caret.pred <- predict(fit.gbm,validation)
table(boost.caret.pred ,validation$coc)
mean(boost.caret.pred==validation$coc)
# 0.5851064


### Random Forest algorithm. ###
fit.rf <- train(coc~., data=dataset, method = 'rf', trControl=fitControl, tuneGrid=rfGrid, metric='Accuracy', distribution='multinomial')
fit.rf

res_rf <- fit.rf$results
acc_rf <- subset(res_rf[2]) 
# Note: accuracy is in the second column!
# CV con mejor "tune" 
max(acc_rf)
# 0.6050778

rf.caret.pred <- predict(fit.rf,validation)

table(rf.caret.pred ,validation$coc)
mean(rf.caret.pred==validation$coc)
# 0.5904255


# RF IS THE WINNER
# GBM, 0.5851064
mean(boost.caret.pred==validation$coc)
# Random forest, 0.5904255
mean(rf.caret.pred==validation$coc)

# In my case RF is the winner and that is what I use below. I have defined the grid above.
# Now I use 80% of the data!
# Important: This example is done on a small sample, and it would be better to use
# nested CV. If we change the seed, we may get another winner.

### Gradient boosting machine algorithm. ###
fit.gbm_total <- train(coc~., data=total, method = 'gbm', trControl=fitControl, tuneGrid=gbmGrid, metric='Accuracy', distribution='multinomial')
fit.gbm_total
plot(fit.gbm_total)
fit.gbm_total$bestTune

res_gbm_total <- fit.gbm_total$results
acc_gbm_total <- subset(res_gbm_total[5])
# CV con mejor "tune"
max(acc_gbm_total)
#0.6140265

#Evaluate on test
boost.caret.pred_total <- predict(fit.gbm_total,test)

table(boost.caret.pred_total ,test$coc)
mean(boost.caret.pred_total==test$coc)
#0.6312997


## RANDOM FOREST ##
fit.rf_total <- train(coc~., data=total, method = 'rf', trControl=fitControl, tuneGrid=rfGrid, metric='Accuracy', distribution='multinomial')
fit.rf_total
res_rf_total <- fit.rf_total$results
acc_rf_total <- subset(res_rf_total[2]) 
# Note: accuracy is in the second column!
# CV con mejor "tune" 
max(acc_rf_total)
# 0.5881917

rf.caret.pred_total <- predict(fit.rf_total,test)

table(rf.caret.pred_total ,test$coc)
mean(rf.caret.pred_total==test$coc)
# 0.6366048

# This is an estimate of the accuracy that we can expect.

