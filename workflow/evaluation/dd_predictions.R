library(h2o)
load(file="~/GitHub/dd_forecast/dataframes/full_data/df_ml1_v2")
# only select confirmed variables
load("~/GitHub/dd_forecast/dataframes/Partition1/variable_selection/VarSel")
df_ml1<-df_ml1[which(names(df_ml1) %in% c(names(df_ml1)[c(2,7,16:25,3233:3236)],names(VarSel$finalDecision)[which(VarSel$finalDecision=="Confirmed")]))]

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

load("~/GitHub/dd_forecast/classifier/v2/Partition1/leaderboard")
head(leaderboard)

# import best performing model
classifier <- h2o.loadModel("classifier/v2/Partition1/h2o/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233")

# import as MOJO:
#classifier <- h2o.import_mojo("classifier/v2/Partition1/MOJO/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233.zip")

# create frame for generating predictions
df_ml1_h2o<-as.h2o(df_ml1)

preds_all <- h2o.predict(classifier, df_ml1_h2o)
preds_all <- cbind(df_ml1[c(1:12,280:283)], as.data.frame(preds_all))

preds<-subset(preds_all, preds_all$category=="DD")
preds_ds<-subset(preds_all, preds_all$category!="DD")

length(which(preds$predict=="threatened"))/nrow(preds)
mean(preds$threatened)
mean(preds_ds$threatened)
length(which(preds$predict=="threatened"))
#save(preds, file="~/GitHub/dd_forecast/dataframes/Partition1/predictions/preds_dd")
load(file="~/GitHub/dd_forecast/dataframes/Partition1/predictions/preds_dd")



