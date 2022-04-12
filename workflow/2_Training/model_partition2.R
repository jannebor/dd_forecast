options(java.parameters = "-Xmx32000m")

library(tidyverse)
library(h2o)

# get training and testing data created in 3_model_prep.R
load(file="~/GitHub/dd_forecast/dataframes/Partition2/train_mar_v2")
load(file="~/GitHub/dd_forecast/dataframes/Partition2/test_mar_v2")
load(file="~/GitHub/dd_forecast/dataframes/Partition2/train_terr_v2")
load(file="~/GitHub/dd_forecast/dataframes/Partition2/test_terr_v2")

load(file="~/GitHub/dd_forecast/dataframes/Partition1/train_df_v2")
load(file="~/GitHub/dd_forecast/dataframes/Partition1/test_df_v2")

# check that species are not used for both, training & testing
which(train_df$binomial %in% test_df$binomial)
which(train_mar$binomial %in% test_mar$binomial)
which(train_terr$binomial %in% test_terr$binomial)

which(test_df$binomial %in% train_df$binomial)
which(test_mar$binomial %in% train_mar$binomial)
which(test_terr$binomial %in% train_terr$binomial)

# PARTITION 2
# only select confirmed variables
load("~/GitHub/dd_forecast/dataframes/Partition2/variable_selection/VarSel_terr")
train_terr<-train_terr[which(names(train_terr) %in% c("category_group",names(VarSel$finalDecision)[which(VarSel$finalDecision=="Confirmed")]))]

load("~/GitHub/dd_forecast/dataframes/Partition2/variable_selection/VarSel_mar")
train_mar<-train_mar[which(names(train_mar) %in% c("category_group",names(VarSel$finalDecision)[which(VarSel$finalDecision=="Confirmed")]))]

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

# create train and test h2o data frames
train_mar_h2o<-as.h2o(train_mar)
test_mar_h2o<-as.h2o(test_mar)
test_df_h2o<-as.h2o(test_df)
test_mar_h2o<-as.h2o(test_mar)
test_terr_h2o<-as.h2o(test_terr)

# Identify predictors and response
y <- "category_group"
x <- setdiff(names(train_mar_h2o), c(y,"category","binomial","order_","family","genus", "needs_update"))

# Number of CV folds
nfolds <- 10

# check if classes are imbalanced
print(h2o.table(train_mar_h2o[paste(y)]))

# define oversampling factors to counterweight imbalance
sample_factors<-c()
for (i in 1:nrow(h2o.table(train_mar_h2o[paste(y)])[2])) {
  sample_factors<-c(sample_factors,
                    (max(h2o.table(train_mar_h2o[paste(y)])[2])/h2o.table(train_mar_h2o[paste(y)])[i,2]))
}

PE_classifier2_mar <- h2o.automl(x = x, y = y,
                            training_frame = train_mar_h2o,
                            max_models = 134,
                            seed = 5,
                            nfolds = nfolds,
                            balance_classes = TRUE,
                            class_sampling_factors = sample_factors,
                            keep_cross_validation_predictions = TRUE,
                            sort_metric = "AUC",
                            project_name = "Partition2_mar",
                            exploitation_ratio = 0.1,
                            verbosity = "info"
)

# Get model ids for all models in the AutoML Leaderboard
leaderboard <- as.data.frame(PE_classifier2_mar@leaderboard)
leaderboard
#save(leaderboard, file="~/GitHub/dd_forecast/classifier/v2/Partition2/mar/leaderboard")

var_ens<-as.data.frame(h2o::h2o.varimp(PE_classifier2_mar@leader@model$metalearner_model))
#save(var_ens, file="~/GitHub/dd_forecast/classifier/v2/Partition2/mar/var_ens")

# save all models
for(i in 1:length(grep("", leaderboard$model_id, value = TRUE))){
  assign("mm", h2o.getModel(grep("", leaderboard$model_id, value = TRUE)[i]))
  h2o.download_mojo(mm, path="~/GitHub/dd_forecast/classifier/v2/Partition2/mar/MOJO")
  h2o::h2o.saveModel(mm, path="~/GitHub/dd_forecast/classifier/v2/Partition2/mar/h2o")
}


train_terr_h2o<-as.h2o(train_terr)
test_terr_h2o<-as.h2o(test_terr)

# Identify predictors and response
y <- "category_group"
x <- setdiff(names(train_terr_h2o), c(y,"category","binomial","order_","family","genus", "needs_update"))

# Number of CV folds
nfolds <- 10

# check if classes are imbalanced
print(h2o.table(train_terr_h2o[paste(y)]))

# define oversampling factors to counterweight imbalance
sample_factors<-c()
for (i in 1:nrow(h2o.table(train_terr_h2o[paste(y)])[2])) {
  sample_factors<-c(sample_factors,
                    (max(h2o.table(train_terr_h2o[paste(y)])[2])/h2o.table(train_terr_h2o[paste(y)])[i,2]))
}

PE_classifier2_terr <- h2o.automl(x = x, y = y,
                             training_frame = train_terr_h2o,
                             max_models = 154,
                             seed = 5,
                             nfolds = nfolds,
                             balance_classes = TRUE,
                             class_sampling_factors = sample_factors,
                             keep_cross_validation_predictions = TRUE,
                             sort_metric = "AUC",
                             project_name = "Partition2_terr",
                             exploitation_ratio = 0.1,
                             verbosity = "info"
)


# Get model ids for all models in the AutoML Leaderboard
leaderboard <- as.data.frame(PE_classifier2_terr@leaderboard)
leaderboard
#save(leaderboard, file="~/GitHub/dd_forecast/classifier/v2/Partition2/terr/leaderboard")

var_ens<-as.data.frame(h2o::h2o.varimp(PE_classifier2_terr@leader@model$metalearner_model))
#save(var_ens, file="~/GitHub/dd_forecast/classifier/v2/Partition2/terr/var_ens")

# save all models
for(i in 1:length(grep("", leaderboard$model_id, value = TRUE))){
  assign("mm", h2o.getModel(grep("", leaderboard$model_id, value = TRUE)[i]))
  h2o.download_mojo(mm, path="~/GitHub/dd_forecast/classifier/v2/Partition2/terr/MOJO")
  h2o::h2o.saveModel(mm, path="~/GitHub/dd_forecast/classifier/v2/Partition2/terr/h2o")
}


classifier_mar <- PE_classifier2_mar@leader
performance_mar <- h2o.performance(classifier_mar, newdata = test_mar_h2o)

classifier_terr <- PE_classifier2_terr@leader
performance_terr <- h2o.performance(classifier_terr, newdata = test_terr_h2o)

