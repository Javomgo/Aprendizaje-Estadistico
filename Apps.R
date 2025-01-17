library(dplyr)
library(tidyr)
library(xgboost)
require("caret")
require("randomForest")
require("gbm")

apps.df = read.csv("googleplaystore.csv")
summary(apps.df)
str(apps.df)
apps.df$Installs[apps.df$Installs == "Free" ] = NA
apps.df$Rating[apps.df$Rating == 19] = NA
apps.df$Size[apps.df$Size == "Varies with device"] = NA
apps.df = mutate(apps.df[,-c(10:13)]) %>% na.omit %>% droplevels()
apps.df = apps.df[!duplicated(apps.df),]
apps.df$Reviews = as.numeric(as.character(apps.df$Reviews))

#Tratamiento del tamaño
apps.df$Size = as.character(apps.df$Size)
size_express_mb <- "[0-9]+[[:punct:]]?[0-9]*M"
size_express_kb <- "[0-9]+[[:punct:]]?[0-9]*k"

index_mb <- grep(apps.df$Size,pattern = size_express_mb)
mb.match = regexpr(apps.df$Size, pattern = size_express_mb)
mb = regmatches(apps.df$Size,mb.match)

mb1 <- substr(mb, 1,nchar(mb) - 1)
mb1 <- as.numeric(mb1)*1000
apps.df$Size[index_mb] <- mb1

index_kb <- grep(apps.df$Size,pattern = size_express_kb)
kb.match = regexpr(apps.df$Size, pattern = size_express_kb)
kb = regmatches(apps.df$Size,kb.match)

kb1 <- substr(kb, 1,nchar(kb) - 1)
kb1 <- as.numeric(kb1)
apps.df$Size[index_kb] <- kb1
apps.df$Size = as.numeric(apps.df$Size)

# Tratamiento de la variable Install

apps.df$Installs = as.character(apps.df$Installs)
apps.df$Installs = substr(apps.df$Installs, 1, nchar(apps.df$Installs)-1)
apps.df$Installs = gsub(",", "", c(apps.df$Installs))
apps.df$Installs = as.numeric(apps.df$Installs)

#Tratamiento de la variable Price

apps.df$Price = as.character(apps.df$Price)
apps.df$Price = substr(apps.df$Price, 2, nchar(apps.df$Price))
apps.df$Price[apps.df$Price == ""] = 0
apps.df$Price = as.numeric(apps.df$Price)

set.seed(2019)




# Make 60% to train, 20% to validate, and 20% to test (your prefered model)
inTraining <- createDataPartition(apps.df$Installs, p=0.6, list=FALSE)
training.set <- apps.df[inTraining,]
Totalvalidation.set <- apps.df[-inTraining,]
# This will create another partition of the 40% of the data, so 20%-testing and 20%-validation
inValidation <- createDataPartition(Totalvalidation.set$Installs, p=0.5, list=FALSE)
testing.set <- Totalvalidation.set[inValidation,]
validation.set <- Totalvalidation.set[-inValidation,]

dataset <- training.set
validation <- validation.set
test <- testing.set

# The final model will actually use all data, except test
total <- rbind(dataset, validation)

# I reduce the grid to save time here. 

fitControl <- trainControl(method = 'cv', number = 5, summaryFunction=defaultSummary)

getModelInfo()$gbm$parameters
gbmGrid <-  expand.grid(interaction.depth = c(1,4,7,10),
                        n.trees = c(500, 1000, 2000),
                        shrinkage = c(.005, .02,.05),
                        n.minobsinnode = 10)
gbmGrid

getModelInfo()$rf$parameters
#mtry max:
ncol(apps.df)-1

rfGrid <-  expand.grid(mtry = c(1,2,3,4,6,9,12))
rfGrid


### Gradient boosting machine algorithm. ###
fit.gbm <- train(Installs~. -App, data=dataset, method = 'gbm', trControl=fitControl, tuneGrid=gbmGrid, metric='RMSE')
fit.gbm
plot(fit.gbm)
fit.gbm$bestTune
fit.gbm$results
res_gbm <- fit.gbm$results
RMSE_gbm <- subset(res_gbm[5])
# CV con mejor "tune"
BCV.RMSE = max(RMSE_gbm)

boost.caret.pred <- predict(fit.gbm,validation)
B.RMSE = (mean((boost.caret.pred - validation$Installs)^2))^0.5

### EXtreme Gradient boosting algorithm. ###

tuneGridXGB <- expand.grid(
  nrounds=c(350),
  max_depth = c(4, 6),
  eta = c(0.05, 0.1),
  gamma = c(0.01),
  colsample_bytree = c(0.75),
  subsample = c(0.50),
  min_child_weight = c(0))

fit.gbmx <- train(Installs~. -App, data=dataset, method = 'xgbTree', trControl=fitControl, tuneGrid=tuneGridXGB, metric='RMSE')
fit.gbmx

fit.gbmx$bestTune
fit.gbmx$results
res_gbmx <- fit.gbmx$results
RMSE_gbmx <- subset(res_gbmx[8])
# CV con mejor "tune"
XBCV.RMSE = max(RMSE_gbmx)

xboost.caret.pred <- predict(fit.gbmx,validation)
XB.RMSE = (mean((xboost.caret.pred - validation$Installs)^2))^0.5

### Random Forest algorithm. ###
fit.rf <- train(Installs~. -App, data=dataset, method = 'rf', trControl=fitControl, tuneGrid=rfGrid, metric='RMSE')
fit.rf

res_rf <- fit.rf$results
RMSE_rf <- subset(res_rf[2]) 
# Note: accuracy is in the second column!
# CV con mejor "tune" 
RFCV.RMSE = max(RMSE_rf)


rf.caret.pred <- predict(fit.rf,validation)
RF.RMSE = (mean((rf.caret.pred - validation$Installs)^2))^0.5

## XGB wins
set.seed(2019)
fit.xgb_total <- train(Installs~. -App, data=total, method = 'xgbTree', trControl=fitControl, tuneGrid=tuneGridXGB, metric='RMSE')
fit.xgb_total
fit.xgb_total$bestTune

res_xgb_total <- fit.xgb_total$results
RMSE_xgb_total <- subset(res_xgb_total[8])
# CV con mejor "tune"
max(RMSE_xgb_total)

#Evaluate on test
xboost.caret.pred_total <- predict(fit.xgb_total,test)
XB_T.RMSE = (mean((xboost.caret.pred_total - test$Installs)^2))^0.5
