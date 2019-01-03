library(dplyr)
library(tidyr)
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
apps.df$Size = as.character(apps.df$Size)

set.seed(11)

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
                        n.trees = c(500,1000,2000),
                        shrinkage = c(.005, .02,.05),
                        n.minobsinnode = 10)
gbmGrid

getModelInfo()$rf$parameters
#mtry max:
ncol(apps.df)-1

rfGrid <-  expand.grid(mtry = c(1,2,3,4,6,9,12))
rfGrid


### Gradient boosting machine algorithm. ###
fit.gbm <- train(Installs~ . -App, data=dataset, method = 'gbm', trControl=fitControl, tuneGrid=gbmGrid, metric='Accuracy', distribution='multinomial')
fit.gbm
plot(fit.gbm)
fit.gbm$bestTune

res_gbm <- fit.gbm$results
acc_gbm <- subset(res_gbm[5])
# CV con mejor "tune"
max(acc_gbm)
# 0.6165743