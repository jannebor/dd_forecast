options(java.parameters = "-Xmx32000m")

library(tidyverse)
library(h2o)

# get training and testing data created in 3_model_prep.R
load(file="~/GitHub/dd_forecast/dataframes/train_mar")
load(file="~/GitHub/dd_forecast/dataframes/test_mar")
load(file="~/GitHub/dd_forecast/dataframes/train_terr")
load(file="~/GitHub/dd_forecast/dataframes/test_terr")
load(file="~/GitHub/dd_forecast/dataframes/train_df")
load(file="~/GitHub/dd_forecast/dataframes/test_df")

# check that species are not used for both, training & testing
which(train_df$binomial %in% test_df$binomial)
which(train_mar$binomial %in% test_mar$binomial)
which(train_terr$binomial %in% test_terr$binomial)

which(test_df$binomial %in% train_df$binomial)
which(test_mar$binomial %in% train_mar$binomial)
which(test_terr$binomial %in% train_terr$binomial)

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

# PARTITION 1
# create train and test h2o data frames
setwd("~/GitHub/dd_forecast/classifier/Partition1")
train_df_h2o<-as.h2o(train_df)
test_df_h2o<-as.h2o(test_df)
test_mar_h2o<-as.h2o(test_mar)
test_terr_h2o<-as.h2o(test_terr)

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

PE_classifier1 <- h2o.automl(x = x, y = y,
                      training_frame = train_df_h2o,
                      max_models = 199,
                      seed = 5,
                      nfolds = nfolds,
                      balance_classes = TRUE,
                      class_sampling_factors = sample_factors,
                      keep_cross_validation_predictions = TRUE,
                      sort_metric = "AUC",
                      project_name = "Partition1",
                      exploitation_ratio = 0.1,
                      verbosity = "info"
)


# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(PE_classifier1@leaderboard$model_id)[,1]

# save all models
for(i in 1:length(grep("", model_ids, value = TRUE))){
  assign("mm", h2o.getModel(grep("", model_ids, value = TRUE)[i]))
  h2o.download_mojo(mm)
}

classifier <- PE_classifier1@leader
performance <- h2o.performance(classifier, newdata = test_df_h2o)
performance_mar <- h2o.performance(classifier, newdata = test_mar_h2o)
performance_terr <- h2o.performance(classifier, newdata = test_terr_h2o)


# PARTITION 2
setwd("~/GitHub/dd_forecast/classifier/Partition2")
train_mar_h2o<-as.h2o(train_mar)
test_mar_h2o<-as.h2o(test_mar)

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
                            max_models = 199,
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
model_ids <- as.data.frame(PE_classifier2_mar@leaderboard$model_id)[,1]

# save all models
for(i in 1:length(grep("", model_ids, value = TRUE))){
  assign("mm", h2o.getModel(grep("", model_ids, value = TRUE)[i]))
  h2o.download_mojo(mm)
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
                             max_models = 199,
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
model_ids <- as.data.frame(PE_classifier2_terr@leaderboard$model_id)[,1]

# save all models
for(i in 1:length(grep("", model_ids, value = TRUE))){
  assign("mm", h2o.getModel(grep("", model_ids, value = TRUE)[i]))
  h2o.download_mojo(mm)
}


classifier_mar <- PE_classifier2_mar@leader
performance_mar <- h2o.performance(classifier_mar, newdata = test_mar_h2o)

classifier_terr <- PE_classifier2_terr@leader
performance_terr <- h2o.performance(classifier_terr, newdata = test_terr_h2o)


###




train_mar<-subset(train_df, train_df$marine == "true")
train_terr<-subset(train_df, train_df$terrestrial == "true")
train_fresh<-subset(train_df, train_df$freshwater == "true")

test_mar<-subset(test_df, test_df$marine == "true")
test_terr<-subset(test_df, test_df$terrestrial == "true")
test_fresh<-subset(test_df, test_df$freshwater == "true")

# PARTITION 3
setwd("~/GitHub/dd_forecast/classifier/Partition3")
train_mar_h2o<-as.h2o(train_mar)
test_mar_h2o<-as.h2o(test_mar)

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

PE_classifier3_mar <- h2o.automl(x = x, y = y,
                                 training_frame = train_mar_h2o,
                                 max_models = 199,
                                 seed = 5,
                                 nfolds = nfolds,
                                 balance_classes = TRUE,
                                 class_sampling_factors = sample_factors,
                                 keep_cross_validation_predictions = TRUE,
                                 sort_metric = "AUC",
                                 project_name = "Partition3_mar",
                                 exploitation_ratio = 0.1,
                                 verbosity = "info"
)


# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(PE_classifier3_mar@leaderboard$model_id)[,1]

# save all models
for(i in 1:length(grep("", model_ids, value = TRUE))){
  assign("mm", h2o.getModel(grep("", model_ids, value = TRUE)[i]))
  h2o.download_mojo(mm)
}


##
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

PE_classifier3_terr <- h2o.automl(x = x, y = y,
                                 training_frame = train_terr_h2o,
                                 max_models = 199,
                                 seed = 5,
                                 nfolds = nfolds,
                                 balance_classes = TRUE,
                                 class_sampling_factors = sample_factors,
                                 keep_cross_validation_predictions = TRUE,
                                 sort_metric = "AUC",
                                 project_name = "Partition3_terr",
                                 exploitation_ratio = 0.1,
                                 verbosity = "info"
)


# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(PE_classifier3_terr@leaderboard$model_id)[,1]

# save all models
for(i in 1:length(grep("", model_ids, value = TRUE))){
  assign("mm", h2o.getModel(grep("", model_ids, value = TRUE)[i]))
  h2o.download_mojo(mm)
}


##
train_fresh_h2o<-as.h2o(train_fresh)
test_fresh_h2o<-as.h2o(test_fresh)

# Identify predictors and response
y <- "category_group"
x <- setdiff(names(train_fresh_h2o), c(y,"category","binomial","order_","family","genus", "needs_update"))

# Number of CV folds
nfolds <- 10

# check if classes are imbalanced
print(h2o.table(train_fresh_h2o[paste(y)]))

# define oversampling factors to counterweight imbalance
sample_factors<-c()
for (i in 1:nrow(h2o.table(train_fresh_h2o[paste(y)])[2])) {
  sample_factors<-c(sample_factors,
                    (max(h2o.table(train_fresh_h2o[paste(y)])[2])/h2o.table(train_fresh_h2o[paste(y)])[i,2]))
}

PE_classifier3_fresh <- h2o.automl(x = x, y = y,
                                 training_frame = train_fresh_h2o,
                                 max_models = 199,
                                 seed = 5,
                                 nfolds = nfolds,
                                 balance_classes = TRUE,
                                 class_sampling_factors = sample_factors,
                                 keep_cross_validation_predictions = TRUE,
                                 sort_metric = "AUC",
                                 project_name = "Partition3_fresh",
                                 exploitation_ratio = 0.1,
                                 verbosity = "info"
)


# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(PE_classifier3_fresh@leaderboard$model_id)[,1]

# save all models
for(i in 1:length(grep("", model_ids, value = TRUE))){
  assign("mm", h2o.getModel(grep("", model_ids, value = TRUE)[i]))
  h2o.download_mojo(mm)
}



classifier_mar <- PE_classifier3_mar@leader
performance_mar <- h2o.performance(classifier_mar, newdata = test_mar_h2o)

classifier_terr <- PE_classifier3_terr@leader
performance_terr <- h2o.performance(classifier_terr, newdata = test_terr_h2o)

classifier_fresh <- PE_classifier3_fresh@leader
performance_fresh <- h2o.performance(classifier_fresh, newdata = test_fresh_h2o)




