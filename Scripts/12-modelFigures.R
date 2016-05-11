# Plots for down models

load("model Data/Final Models/Down all list.Rdata")

fw <- 125
fh <- 94


for(i in (1:13)[-4]){
     png(paste0("model Data/Figures/Down/roc/",names(down_models[i]),"-roc.png"),width = fw,height = fh,units = "mm",res = 300)
     print(plot(down_models[[i]],metric = "ROC",main = names(down_models[i])))
     dev.off()
}

for(i in (1:13)[-4]){
     png(paste0("model Data/Figures/Down/",names(down_models[i]),"-sens.png"),width = fw,height = fh,units = "mm",res = 300)
     print(plot(down_models[[i]],metric = "Sens",main = names(down_models[i])))
     dev.off()
}


# Variable Importance
imps_dn <- lapply(down_models,varImp)
for(i in (1:13)[-4]){
     png(paste0("model Data/Figures/Down/Vars/",names(down_models[i]),"-var.png"),width = fw,height = fh,units = "mm",res = 300)
     print(plot(imps_dn[[1]],main = paste0("Top predictors for model:",names(down_models[i]))))
     dev.off()
}

# pretty much everything was same, so no need.


# Plots for up models

load("model Data/Final Models/up all list.Rdata")




for(i in (1:11)[-4]){
     png(paste0("model Data/Figures/Up/",names(up_models[i]),"-ROC.png"),width = fw,height = fh,units = "mm",res = 300)
     print(plot(up_models[[i]],metric = "ROC",main = names(up_models[i])))
     dev.off()
}

for(i in (1:11)[-4]){
     png(paste0("model Data/Figures/Up/sens/",names(up_models[i]),"-sens.png"),width = fw,height = fh,units = "mm",res = 300)
     print(plot(up_models[[i]],metric = "Sens",main = names(up_models[i])))
     dev.off()
}