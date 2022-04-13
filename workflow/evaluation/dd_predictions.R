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

preds_all <- h2o.predict(classifier, df_ml1_h2o)
preds_all <- cbind(df_ml1[c(1:12,280:283)], as.data.frame(preds_all))

# only select Data Deficient species and save as R object
preds<-subset(preds_all, preds_all$category=="DD")
#save(preds, file="dataframes/Partition1/predictions/preds_dd")
load(file="dataframes/Partition1/predictions/preds_dd")

# only select Data sufficient species
preds_ds<-subset(preds_all, preds_all$category!="DD")

# fraction of Data Deficient species predicted to be threatened
length(which(preds$predict=="threatened"))/nrow(preds)
# number of Data Deficient species predicted to be threatened
length(which(preds$predict=="threatened"))

# average predicted score for Data Deficient species
mean(preds$threatened)
# average predicted score for Data sufficient species
mean(preds_ds$threatened)

# end h2o session
h2o::h2o.shutdown(prompt = F)