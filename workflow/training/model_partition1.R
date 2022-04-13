#options(java.parameters = "-Xmx32000m")

library(tidyverse)
library(h2o)

# PARTITION 1
# get training and testing data created in model_prep.R
# training data
load(file="dataframes/Partition1/train_df_v2")


# testing data
load(file="dataframes/Partition1/test_df_v2")


# check that species are not used for both, training & testing
which(train_df$binomial %in% test_df$binomial)
which(test_df$binomial %in% train_df$binomial)



# only select confirmed variables in training data
load("dataframes/Partition1/variable_selection/VarSel")
train_df<-train_df[which(names(train_df) %in% c("category_group",names(VarSel$finalDecision)[which(VarSel$finalDecision=="Confirmed")]))]




# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

# create train and test h2o data frames
train_df_h2o<-as.h2o(train_df)
test_df_h2o<-as.h2o(test_df)


# Identify predictors and response
y <- "category_group"
x <- setdiff(names(train_df_h2o), c(y,"category","binomial","order_","family","genus", "needs_update"))

# Number of CV folds
nfolds <- 10

# check if classes are imbalanced
print(h2o.table(train_df_h2o[paste(y)]))

# define oversampling factors to counterweight imbalance
sample_factors<-c()
for (i in 1:nrow(h2o.table(train_df_h2o[paste(y)])[2])) {
  sample_factors<-c(sample_factors,
                    (max(h2o.table(train_df_h2o[paste(y)])[2])/h2o.table(train_df_h2o[paste(y)])[i,2]))
}

# run AutoML
PE_classifier1 <- h2o.automl(x = x, y = y,
                             training_frame = train_df_h2o,
                             max_models = 222,
                             seed = 5,
                             nfolds = nfolds,
                             balance_classes = TRUE,
                             leaderboard_frame = test_df_h2o,
                             class_sampling_factors = sample_factors,
                             keep_cross_validation_predictions = TRUE,
                             exclude_algos = c("XGBoost"),
                             sort_metric = "AUC",
                             project_name = "Partition1",
                             exploitation_ratio = 0.1,
                             verbosity = "info"
)


# Get AutoML Leaderboard
leaderboard <- as.data.frame(PE_classifier1@leaderboard)
leaderboard
#save(leaderboard, file="classifier/v2/Partition1/leaderboard")

# get importance of base-learners for best performing model (if stacked ensemble)
var_ens<-as.data.frame(h2o::h2o.varimp(PE_classifier1@leader@model$metalearner_model))
#save(var_ens, file="classifier/v2/Partition1/var_ens")

# save all models
#for(i in 1:length(grep("", leaderboard$model_id, value = TRUE))){
#  assign("mm", h2o.getModel(grep("", leaderboard$model_id, value = TRUE)[i]))
#  h2o.download_mojo(mm, path="classifier/v2/Partition1/MOJO")
#  h2o::h2o.saveModel(mm, path="classifier/v2/Partition1/h2o")
#}

# end h2o session
h2o::h2o.shutdown(prompt = F)