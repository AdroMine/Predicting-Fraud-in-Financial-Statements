# Imbalanced Classes machine Learning

require(caret)

fullData <- read.csv("model Data/newVarMScore.csv")
fullData <- fullData[,-(1:3)]
fullData$mfraud <- NULL
fullData$fraud <- relevel(fullData$fraud,"yes")

set.seed("6345") # setting seed for reproducibility

# first parition of 80:20 for training&validation on one group, and test on another
inTrain <- createDataPartition(fullData$fraud,p=0.8,list = FALSE)

# create train and test data set
training <- fullData[inTrain,]
# validation <- firstsplit[-part2,]
testing <- fullData[-inTrain,]

# imbalance dataset resampling to set it right
set.seed(5839)
down_training <- downSample(training[,-ncol(training)],training$fraud)
up_training <- upSample(training[,-ncol(training)],training$fraud)


ctrl_up <- trainControl(method = "repeatedcv", repeats = 5,number = 3,classProbs = TRUE,
                     summaryFunction = twoClassSummary,verboseIter = TRUE)

ctrl_dn <- trainControl(method = "repeatedcv", repeats = 5,number = 3,classProbs = TRUE,
                     summaryFunction = twoClassSummary,verboseIter = TRUE)

# Attempt 1 ada boosted classification tree ! UD

ada_up <- train(Class~.,data = up_training,method = "ada",metric = "ROC",trControl = ctrl_up)

ada_down <- train(Class~.,data = down_training,method = "ada",metric = "ROC",trControl = ctrl_dn)


# Bagged CART ! Done both UD

bcart_up <- train(Class~.,data = up_training,method = "treebag",metric = "ROC",trControl = ctrl_up)
bcart_dn <- train(Class~.,data = down_training,method = "treebag",metric = "ROC",trControl = ctrl_up)


# Boosted generalised linear model ! Done UD 
bglm_up <- train(Class~.,data = up_training,method = "glmboost",metric = "ROC",trControl = ctrl_up)
bglm_dn <- train(Class~.,data = down_training,method = "glmboost",metric = "ROC",trControl = ctrl_up)



# Boosted Logistic Regression ! UD
blg_up <- train(Class~.,data = up_training,method = "LogitBoost",metric = "ROC",trControl = ctrl_up)
blg_dn <- train(Class~.,data = down_training,method = "LogitBoost",metric = "ROC",trControl = ctrl_dn)


# C5.0 ! Done UD

c5_up <- train(Class~.,data = up_training,method = "C5.0",metric = "ROC",trControl = ctrl_up)
c5_dn <- train(Class~.,data = down_training,method = "C5.0",metric = "ROC",trControl = ctrl_up)


# Model Averaged Neural Network ! Done UD 

ann_up <- train(Class~.,data = up_training,method = "avNNet",metric = "ROC",trControl = ctrl_up)
ann_dn <- train(Class~.,data = down_training,method = "avNNet",metric = "ROC",trControl = ctrl_up)


# Parallel Random Forest ! Done UD

rf_up <- train(Class~.,data = up_training,method = "parRF",metric = "ROC",trControl = ctrl_up)
rf_dn <- train(Class~.,data = down_training,method = "parRF",metric = "ROC",trControl = ctrl_up)

# above all done Up and down sample both


# AdaBoost.M1 ! Done UD
adbo_up <- train(Class~.,data = up_training,method = "AdaBoost.M1",metric = "ROC",trControl = ctrl_up)
adbo_dn <- train(Class~.,data = down_training,method = "AdaBoost.M1",metric = "ROC",trControl = ctrl_up)



# Bagged MARS ! Done D
bmars_up <- train(Class~.,data = up_training,method = "bagEarth",metric = "ROC",trControl = ctrl_up)
bmars_dn <- train(Class~.,data = down_training,method = "bagEarth",metric = "ROC",trControl = ctrl_up)


# Boosted Generalized Additive Model ! Done D
bgam_up <- train(Class~.,data = up_training,method = "gamboost",metric = "ROC",trControl = ctrl_up)
bgam_dn <- train(Class~.,data = down_training,method = "gamboost",metric = "ROC",trControl = ctrl_up)


# Ensembles of Generalized Linear Models ! D
eglm_up <- train(Class~.,data = up_training,method = "randomGLM",metric = "ROC",trControl = ctrl_up)
eglm_dn <- train(Class~.,data = down_training,method = "randomGLM",metric = "ROC",trControl = ctrl_up)



# eXtreme Gradient Boosting UD
egb_up <- train(Class~.,data = up_training,method = "xgbTree",metric = "ROC",trControl = ctrl_up)
egb_dn <- train(Class~.,data = down_training,method = "xgbTree",metric = "ROC",trControl = ctrl_up)


# Stochastic Gradient Boosting UD
sgb_up <- train(Class~.,data = up_training,method = "gbm",metric = "ROC",trControl = ctrl_up)
sgb_dn <- train(Class~.,data = down_training,method = "gbm",metric = "ROC",trControl = ctrl_up)


# svm trial 1
library(e1071)
svmfit1 <- svm(Class~.,data = up_training,scale = FALSE,
               kernel = "sigmoid",gamma = 0.000001,cost = 10000)
pred1 <- predict(svmfit1,testing[,1:17])
confusionMatrix(pred1,testing$fraud)

confusionMatrix(predict(svmfit1,up_training[,1:17]),up_training$Class)

svmfit2 <- svm(Class~.,data = down_training,scale = FALSE,
               kernel = "sigmoid",gamma = 1e-5,cost = 100)
pred2 <- predict(svmfit2,testing[,1:17],ty)
confusionMatrix(pred2,testing$fraud)

confusionMatrix(predict(svmfit2,up_training[,1:17]),up_training$Class)
