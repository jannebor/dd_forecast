library(h2o)

# permutation variable importance for partition 1
load(file="dataframes/Partition1/test_df_v2")

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

# create h2o dataframe
test_df_h2o<-as.h2o(test_df)

# import best performing model (only functional in h2o version 3.36.0.4)
classifier <- h2o.loadModel("classifier/v2/Partition1/h2o/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233")
# else import from MOJO:
#classifier <- h2o.import_mojo("classifier/v2/Partition1/MOJO/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233.zip")

# permutation variable importance in 50 runs
prmi<-h2o.permutation_importance(classifier, test_df_h2o, n_repeats = 50)
#save(prmi,file="classifier/v2/Partition1/prmi50r")
load(file="classifier/v2/Partition1/prmi50r")
head(prmi)

n_features<-25
# plot importance of top 25
vi <- as.data.frame(prmi)
n_repeats<-ncol(prmi)-1
title <- paste("Permutation Variable Importance: Stacked Ensemble",sep = "")
# use the longest ylable to adjust margins so ylabels don't cut off long string labels
ylabels <- as.character(as.list(vi$Variable))
ymargin <- max(strwidth(ylabels, "inch") + 0.01, na.rm = TRUE)
op <- par(mai = c(1.02, ymargin, 0.42, 0.42))
on.exit(par(op))
varimp_order <- order(apply(vi[, -1], 1, mean))
vi <- tail(vi[varimp_order,], n = n_features)
ylabels <- as.character(as.list(vi$Variable))
ymargin <- max(strwidth(ylabels, "inch") + 0.4, na.rm = TRUE)
op <- par(mai = c(1.02, ymargin, 0.42, 0.42))
on.exit(par(op))
  
graphics::boxplot(t(vi[, -1]), names = vi$Variable, horizontal = TRUE, col = "white", yaxt = "n", main = title,
                  xlab = "Permutation Variable Importance", axes = FALSE)
graphics::axis(1)
graphics::axis(2, at = seq_along(vi$Variable),labels = vi$Variable, las = 2, lwd = 0)

# end h2o session
h2o::h2o.shutdown(prompt = F)