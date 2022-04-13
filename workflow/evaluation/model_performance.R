library(h2o)

# get testing data created in model_prep.R
load(file="dataframes/Partition2/test_terr_v2")
load(file="dataframes/Partition2/test_mar_v2")
load(file="dataframes/Partition1/test_df_v2")

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

# create h2o dataframes
test_df_h2o<-as.h2o(test_df)
test_mar_h2o<-as.h2o(test_mar)
test_terr_h2o<-as.h2o(test_terr)


# select best performing partition:
#Partition 1:
classifier1 <- h2o.import_mojo("classifier/v2/Partition1/MOJO/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233.zip")
# calculate performance for marine species
performance_mar1 <- h2o.performance(classifier1, newdata = test_mar_h2o)
# calculate performance for non-marine species
performance_terr1 <- h2o.performance(classifier1, newdata = test_terr_h2o)

#Partition 2:
classifier2_mar <- h2o.import_mojo("classifier/v2/Partition2/mar/MOJO/GBM_grid__1_AutoML_20220403_101223_model_11.zip")
classifier2_terr <- h2o.import_mojo("classifier/v2/Partition2/terr/MOJO/StackedEnsemble_AllModels_AutoML_20220402_113542.zip")
# calculate performance for marine species
performance_mar2 <- h2o.performance(classifier2_mar, newdata = test_mar_h2o)
# calculate performance for non-marine species
performance_terr2 <- h2o.performance(classifier2_terr, newdata = test_terr_h2o)

#Partition 1 performs slightly better on average AUC at equal weight of marine and non-marine species:
print(paste("Partition 1:", mean(c(performance_mar1@metrics$AUC,performance_terr1@metrics$AUC))))
print(paste("Partition 2:", mean(c(performance_mar2@metrics$AUC,performance_terr2@metrics$AUC))))


# Performance of partition 1:
# calculate performance based on all species
performance <- h2o.performance(classifier1, newdata = test_df_h2o)
# calculate performance based on marine species
performance_mar <- h2o.performance(classifier1, newdata = test_mar_h2o)
# calculate performance based on non-marine species
performance_terr <- h2o.performance(classifier1, newdata = test_terr_h2o)

# making predictions on the test data for confusion matrix and plot
preds_ens <- h2o.predict(classifier1, test_df_h2o)
preds_ens<-cbind(test_df[c(1,5:14,3220,3221)], as.data.frame(preds_ens))
#save(preds_ens,file="~/GitHub/dd_forecast/dataframes/Partition1/predictions/preds_testdata")
load(file="dataframes/Partition1/predictions/preds_testdata")
preds_ens<-subset(preds_ens, preds_ens$needs_update!="Yes")


preds_ens_mar<-subset(preds_ens, tolower(preds_ens$marine)=="true")
preds_ens_nonmar<-subset(preds_ens, tolower(preds_ens$marine)!="true")

library(caret)
# confusion matrix for all species
confusionMatrix(factor(preds_ens$predict), factor(preds_ens$category_group))
# confusion matrix for marine species
confusionMatrix(factor(preds_ens_mar$predict), factor(preds_ens_mar$category_group))
# confusion matrix for non-marine species
confusionMatrix(factor(preds_ens_nonmar$predict), factor(preds_ens_nonmar$category_group))

library(viridis)
# boxplot of predicted scores per class (threatened/non threatened) for marine/non marine species 
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



# assess threshold independent performance of partition 1 per taxonomic class
performance_class<-data.frame()
for(k in 1:length(unique(test_df$class))){
  
  # subset taxonomic class k
  testsub<-subset(test_df, test_df$class==unique(test_df$class)[k])
  
  AUC<-NA
  AUCPR<-NA
  class<-as.character(unique(test_df$class)[k])
  n<-length(unique(testsub$binomial))
  
  if(nrow(testsub)>0){
    testsub_h2o<-as.h2o(testsub)
    perf_all_cf <- h2o.performance(classifier1, newdata = testsub_h2o)
    
    if(length(perf_all_cf@metrics$AUC)>0){    
      # retrieve AUC
      AUC<-perf_all_cf@metrics$AUC
    }
    
    if(length(perf_all_cf@metrics$pr_auc)>0){    
      # retrieve AUCPR
      AUCPR<-perf_all_cf@metrics$pr_auc
    }
    
  }
  
  performance_class<-rbind(performance_class,data.frame(
    AUC,
    AUCPR,
    n,
    class
  ))
  print(k)
}
performance_class


# end h2o session
h2o::h2o.shutdown(prompt = F)