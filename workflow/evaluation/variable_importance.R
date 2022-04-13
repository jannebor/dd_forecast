library(h2o)

load(file="~/GitHub/dd_forecast/dataframes/Partition1/test_df_v2")

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

test_df_h2o<-as.h2o(test_df)

load("~/GitHub/dd_forecast/classifier/v2/Partition1/leaderboard")
head(leaderboard)

# import best performing model
classifier <- h2o.loadModel("classifier/v2/Partition1/h2o/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233")

prmi<-h2o.permutation_importance(classifier, test_df_h2o, n_repeats = 50)
#save(prmi,file="/classifier/v2/Partition1/prmi50r")
load(file="classifier/v2/Partition1/prmi50r")
head(prmi)

