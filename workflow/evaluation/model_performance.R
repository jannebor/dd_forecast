library(h2o)

# get testing data created in 3_model_prep.R
load(file="~/GitHub/dd_forecast/dataframes/Partition2/test_terr_v2")
load(file="~/GitHub/dd_forecast/dataframes/Partition2/test_mar_v2")
load(file="~/GitHub/dd_forecast/dataframes/Partition1/test_df_v2")

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

test_df_h2o<-as.h2o(test_df)
test_mar_h2o<-as.h2o(test_mar)
test_terr_h2o<-as.h2o(test_terr)

load("~/GitHub/dd_forecast/classifier/v2/Partition1/leaderboard")
head(leaderboard)

# import best performing model
classifier <- h2o.loadModel("classifier/v2/Partition1/h2o/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233")

# import as MOJO:
#classifier <- h2o.import_mojo("classifier/v2/Partition1/MOJO/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233.zip")

performance <- h2o.performance(classifier, newdata = test_df_h2o)
performance_mar <- h2o.performance(classifier, newdata = test_mar_h2o)
performance_terr <- h2o.performance(classifier, newdata = test_terr_h2o)

performance_mar


# getting predictions of the test data for confusion matrix and plot
preds_ens <- h2o.predict(classifier, test_df_h2o)
preds_ens<-cbind(test_df[c(1,5:14,3220,3221)], as.data.frame(preds_ens))
#save(preds_ens,file="~/GitHub/dd_forecast/dataframes/Partition1/predictions/preds_testdata")
load(file="~/GitHub/dd_forecast/dataframes/Partition1/predictions/preds_testdata")
preds_ens<-subset(preds_ens, preds_ens$needs_update!="Yes")


preds_ens_mar<-subset(preds_ens, tolower(preds_ens$marine)=="true")
preds_ens_nonmar<-subset(preds_ens, tolower(preds_ens$marine)!="true")

library(caret)
confusionMatrix(factor(preds_ens$predict), factor(preds_ens$category_group))
confusionMatrix(factor(preds_ens_mar$predict), factor(preds_ens_mar$category_group))
confusionMatrix(factor(preds_ens_nonmar$predict), factor(preds_ens_nonmar$category_group))

library(viridis)
opar<-par()
par(mar=c(4,4,3,0.5))
par(mfrow=c(1,2))
a<-plot(factor(preds_ens_mar$category_group), preds_ens_mar$threatened,col=inferno(5, direction = -1)[c(1,3)], main="marine species",ylab="Predicted PE score",xlab="",cex=0.4,cex.lab=0.8,cex.axis=0.7,cex.main=0.8, ylim=c(0,1))
mtext(paste("n = ", a$n, sep=""), side=1, at=c(1:length(a$n)),cex=0.55, font=1, padj = 5) 
b<-plot(factor(preds_ens_nonmar$category_group), preds_ens_nonmar$threatened, col=inferno(5, direction = -1)[c(1,3)],ylab="",cex=0.4,cex.lab=0.8,cex.axis=0.7,cex.main=0.8, main="non-marine species",xlab="", ylim=c(0,1))
mtext(paste("n = ", b$n, sep=""), side=1, at=c(1:length(b$n)),cex=0.55, font=1, padj = 5) 
par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(4, 1, 1, 1), new=TRUE)
plot(factor(preds_ens_nonmar$category_group), preds_ens_nonmar$threatened, cex.lab=0.8,yaxt="n",frame=F, xaxt="n",xlab="Actual IUCN assessment", ylim=c(0,0.0001))
par(opar)



# per taxonomic class
perclass<-data.frame()

for(k in 1:length(unique(test_df$class))){
  
  testsub<-subset(test_df, test_df$class==unique(test_df$class)[k])
  
  AUC<-NULL
  AUCPR<-NULL
  n<-0
  class<-as.character(unique(test_df$class)[k])
  
  if(nrow(testsub)>0){
    testsub_h2o<-as.h2o(testsub)
    perf_all_cf <- h2o.performance(classifier, newdata = testsub_h2o)
    
    if(length(perf_all_cf@metrics$AUC)>0){    AUC<-perf_all_cf@metrics$AUC
    }
    
    if(length(perf_all_cf@metrics$pr_auc)>0){    AUCPR<-perf_all_cf@metrics$pr_auc
    }
    
    n<-length(which(testsub$category_group=="threatened"))
  }
  
  
  
  perclass<-rbind(perclass,data.frame(
    
    AUC,
    AUCPR,
    n,
    class
  ))
  
  print(k)
  
}

perclass



perclass$AUC<-round(perclass$AUC,2)
perclass$AUCPR<-round(perclass$AUCPR,2)










