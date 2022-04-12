prmi_plot <- function(prmi, num_of_features) {
  
  vi <- as.data.frame(prmi)
  
  n_repeats<-ncol(prmi)-1
  
  # check the model type and then update the model title
  title <- paste("Permutation Variable Importance: Stacked Ensemble",sep = "")
  title<-NULL
  # use the longest ylable to adjust margins so ylabels don't cut off long string labels
  ylabels <- as.character(as.list(vi$Variable))
  ymargin <- max(strwidth(ylabels, "inch") + 0.01, na.rm = TRUE)
  op <- par(mai = c(1.02, ymargin, 0.42, 0.42))
  on.exit(par(op))
  if (n_repeats > 1) {
    varimp_order <- order(apply(vi[, -1], 1, mean))
    vi <- tail(vi[varimp_order,], n = num_of_features)
    ylabels <- as.character(as.list(vi$Variable))
    ymargin <- max(strwidth(ylabels, "inch") + 0.4, na.rm = TRUE)
    op <- par(mai = c(1.02, ymargin, 0.42, 0.42))
    on.exit(par(op))
    
    graphics::boxplot(t(vi[, -1]), names = vi$Variable, horizontal = TRUE, col = "white", yaxt = "n", main = title,
                      xlab = "Permutation Variable Importance", axes = FALSE)
    graphics::axis(1)
    graphics::axis(2, at = seq_along(vi$Variable), labels = vi$Variable, las = 2, lwd = 0)
  } else {
    # if num_of_features = 1, create only one bar (adjust size to look nice)
    if (num_of_features == 1) {
      graphics::barplot(rev(head(vi[["Scaled Importance"]], n = num_of_features)),
                        names.arg = rev(head(vi$Variable, n = num_of_features)),
                        width = 0.2,
                        space = 1,
                        horiz = TRUE, las = 2,
                        ylim = c(0, 2),
                        xlim = c(0, 1),
                        axes = TRUE,
                        col = '#1F77B4',
                        main = title)
    } else if (num_of_features > 1) {
      graphics::barplot(rev(head(vi[["Scaled Importance"]], n = num_of_features)),
                        names.arg = rev(head(vi$Variable, n = num_of_features)),
                        space = 1, las = 2,
                        horiz = TRUE,
                        col = '#1F77B4', # blue
                        main = title)
    }
  }
  invisible(vi)
}

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

prmi_plot(prmi, num_of_features = 25)

# translate variable names
prmi$Variable[1]<-"Taxonomic class"
prmi$Variable[2]<-"# Native countries"
prmi$Variable[3]<-"Species range extent"
prmi$Variable[4]<-"# Dams within range"
prmi$Variable[6]<-"# Occurrence cells"
prmi$Variable[5]<-"Road density (Min. across range)"
prmi$Variable[7]<-"Occupied environmental domains"
prmi$Variable[8]<-"# Powerplants within range"
prmi$Variable[9]<-"HMI (Min. across range)"
prmi$Variable[10]<-"HFI (Min. across range)"
prmi$Variable[11]<-"ESA LC50 (Max. across range)"
prmi$Variable[12]<-"BIOCLIM 3 (Max. across range)"
prmi$Variable[13]<-"Habitat Correlation (Min. across range)"
prmi$Variable[14]<-"ESA LC40  (Max. across range)"
prmi$Variable[15]<-"CPI (Max. across range)"
prmi$Variable[16]<-"Habitat Variation Coef. (Min. across range)"
prmi$Variable[17]<-"ESA LC40 (Min. across range)"
prmi$Variable[19]<-"BIOCLIM 15 (Min. across range)"
prmi$Variable[22]<-"Habitat Correlation (Max. across range)"
prmi$Variable[23]<-"ESA LC30 (Min. across range)"
prmi$Variable[27]<-"BIOCLIM 19 (Min. across range)"
prmi$Variable[28]<-"Range ocean surface PP (Max. across range)"
prmi$Variable[31]<-"ESA LC50 (Median across range)"
prmi$Variable[50]<-"PA fraction (Max. across range)"
prmi$Variable[36]<-"DDF (Median across range)"

#save figure
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig8.png"
png(pngfile, width=16*1.5, height=15*1.5, units = "cm", res=900)
par(mar=c(0,0,0,0))
par(mfrow=c(1,1))
prmi_plot(prmi, num_of_features = 25)
dev.off()
par(mfrow=c(1,1))

