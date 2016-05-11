# checking models now !!!

down_models <- list(ada = ada_down, 
                    adaBoostM1 = adbo_dn,
                    ANN = ann_dn,
                    BagCart = bcart_dn,
                    boostedGAM = bgam_dn,
                    boostedGLM = bglm_dn ,
                    boostedLR = blg_dn,
                    baggedMARS = bmars_dn,
                    C5 = c5_dn,
                    eXgradBoost = egb_dn, 
                    ensembleGLM = eglm_dn, 
                    randomForest = rf_dn,
                    stochasticGradBoosting = sgb_dn
                    )
inside_resampling <- resamples(down_models)

# function returns ROC for a model and dataset
test_roc <- function(model, data) {
    library(pROC)
    roc_obj <- roc(data$fraud,
                   predict(model, data, type = "prob")[, "yes"])
    ci(roc_obj)
}


inside_ROC <- lapply(down_models, test_roc, data = testing)

inside_ROC <- lapply(inside_ROC, as.vector)
inside_ROC <- do.call("rbind", inside_ROC)
colnames(inside_ROC) <- c("lower", "ROC", "upper")
inside_ROC <- as.data.frame(inside_ROC)

summary(inside_resampling, metric = "ROC")
inside_ROC

cInTables <- lapply(down_models,test_Sens,testing)
ConfInVals <- CtableToDF(cInTables)


# Upsampled models
up_models <- list(ada = ada_up, 
                    adaBoostM1 = adbo_up,
                    ANN = ann_up,
                    BagCart = bcart_up,
                    boostedGAM = bgam_up,
                    boostedGLM = bglm_up ,
                    boostedLR = blg_up,
                    # baggedMARS = bmars_up,
                    C5 = c5_up,
                    eXgradBoost = egb_up, 
                    # ensembleGLM = eglm_up, 
                    randomForest = rf_up,
                    stochasticGradBoosting = sgb_up
)

up_resampling <- resamples(up_models)

up_ROC <- lapply(up_models, test_roc, data = testing)

up_ROC <- lapply(up_ROC, as.vector)
up_ROC <- do.call("rbind", up_ROC)
colnames(up_ROC) <- c("lower", "ROC", "upper")
up_ROC <- as.data.frame(up_ROC)

summary(up_resampling, metric = "ROC")
up_ROC

test_Sens <- function(model,data){
     pred <- caret::predict.train(model,data[,-ncol(data)])
     cfm <- caret::confusionMatrix(pred,data[,ncol(data)])
     cfm
}

cUpTables <- lapply(up_models,test_Sens,testing)
ConfUPVals <- CtableToDF(cUpTables)


CtableToDF <- function(confusionTables){
     classVals <- lapply(confusionTables,function(x) x$byClass)
     nam <- names(classVals[[1]])
     classVals <- lapply(classVals,as.vector)
     classVals <- do.call("rbind",classVals)
     colnames(classVals) <- nam
     classVals <- as.data.frame(classVals)
     classVals
}


### SVMSs
load("D:/RData/Dissertation/model Data/svmModels.Rdata")
pred1 <- predict(svmfit1,testing[,-18])
pred2 <- predict(svmfit2,testing[,-18])
cfm1 <- confusionMatrix(pred1,testing$fraud)
cfm2 <- confusionMatrix(pred2,testing$fraud)
cfms <- list(SVM1 = cfm1, SVM2 = cfm2)
ConfSVMVals <- CtableToDF(cfms)



### Get Predictors!!!!



