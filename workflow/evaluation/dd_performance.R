library(h2o)

# load data frame including all species
load(file="dataframes/full_data/df_ml1_v2")
# only select confirmed variables
load("dataframes/Partition1/variable_selection/VarSel")
df_ml1<-df_ml1[which(names(df_ml1) %in% c(names(df_ml1)[c(2,7,16:25,3233:3236)],names(VarSel$finalDecision)[which(VarSel$finalDecision=="Confirmed")]))]

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

# import best model of partition 1 from MOJO:
classifier <- h2o.import_mojo("classifier/v2/Partition1/MOJO/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233.zip")

# create frame for generating predictions
df_ml1_h2o<-as.h2o(df_ml1)

# generate predictions for all species
preds_all <- h2o.predict(classifier, df_ml1_h2o)
preds_all <- cbind(df_ml1[c(1:12,280:283)], as.data.frame(preds_all))

# select Data Deficient species
preds<-subset(preds_all, preds_all$category=="DD")

#save(preds, file="dataframes/Partition1/predictions/preds_dd")
load(file="dataframes/Partition1/predictions/preds_dd")

# check for re-evaluated data-deficient species
library(rredlist)

# requires IUCN API access
if(!exists("iucn_key_n")){
  if(file.exists("files/iucn_key_n")){
    load("files/iucn_key_n")
  } else {
    iucn_key_n <- rstudioapi::askForPassword("iucn_key_n")
  }
}

# download current RL categories for all Data Defcient species
preds$category_new<-NA
for (i in 1:nrow(preds)) {
  print(i)
  cats<-rredlist::rl_history(as.character(preds$binomial[i]), key=iucn_key_n)
  if(length(cats$result)>0){
    preds$category_new[i]<-cats$result$code[1]
  }
}

# only select species that are not Data Deficient anymore
preds_upd <- subset(preds, preds$category_new!="DD")
preds_upd$category_group_new<-"threatened"
preds_upd$category_group_new[which(preds_upd$category_new %in% c("LC","NT"))] <- "not threatened"
preds_upd$category_new<-factor(preds_upd$category_new, levels = c("LC","NT","VU","EN","CR"))
preds_upd$count<-1

#save(preds_upd, file="dataframes/Partition1/predictions/preds_dd_2021_1")
#save(preds_upd, file="dataframes/Partition1/predictions/preds_dd_2021_2")
load(file="dataframes/Partition1/predictions/preds_dd_2021_1")
load(file="dataframes/Partition1/predictions/preds_dd_2021_2")

# generat plot for overview
preds_upd_mar<-subset(preds_upd, tolower(preds_upd$marine)=="true")
preds_upd_nonmar<-subset(preds_upd, tolower(preds_upd$marine)!="true")

library(viridis)
# boxplot of predicted scores per class (threatened/not threatened) for marine/non-marine species
opar<-par()
par(mar=c(4,4,3,0.5))
par(mfrow=c(1,2))
a<-plot(factor(preds_upd_mar$category_group_new), preds_upd_mar$threatened,col=inferno(5, direction = -1)[c(1,3)], main="marine species",ylab="Predicted PE score",xlab="",cex=0.4,cex.lab=0.8,cex.axis=0.7,cex.main=0.8, ylim=c(0,1))
mtext(paste("n = ", a$n, sep=""), side=1, at=c(1:length(a$n)),cex=0.55, font=1, padj = 5) 
b<-plot(factor(preds_upd_nonmar$category_group_new), preds_upd_nonmar$threatened, col=inferno(5, direction = -1)[c(1,3)],ylab="",cex=0.4,cex.lab=0.8,cex.axis=0.7,cex.main=0.8, main="non-marine species",xlab="", ylim=c(0,1))
mtext(paste("n = ", b$n, sep=""), side=1, at=c(1:length(b$n)),cex=0.55, font=1, padj = 5) 
par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(4, 1, 1, 1), new=TRUE)
plot(factor(preds_upd_nonmar$category_group), preds_upd_nonmar$threatened, cex.lab=0.8,yaxt="n",frame=F, xaxt="n",xlab="Actual IUCN assessment", ylim=c(0,0.0001))
par(opar)

library(caret)
# confusion matrix for marine species
confusionMatrix(factor(preds_upd_mar$predict), factor(preds_upd_mar$category_group_new))
# confusion matrix for non-marine species
confusionMatrix(factor(preds_upd_nonmar$predict), factor(preds_upd_nonmar$category_group_new))
# confusion matrix for all species
confusionMatrix(factor(preds_upd$predict), factor(preds_upd$category_group_new))

# end h2o session
h2o::h2o.shutdown(prompt = F)