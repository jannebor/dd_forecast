library(h2o)
load(file="~/GitHub/dd_forecast/dataframes/df_ml1")
# only for data-deficient species
df_ml1<-subset(df_ml1, df_ml1$category=="DD")

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

# import best performing model
classifier <- h2o.import_mojo("https://github.com/jannebor/dd_forecast/raw/main/classifier/Partition1/StackedEnsemble.zip")

# create frame for generating predictions
df_ml1_h2o<-as.h2o(df_ml1)

preds <- h2o.predict(classifier, df_ml1_h2o)
preds <- cbind(df_ml1[c(2,7,16:25,3211:3214)], as.data.frame(preds))

# check for re-evaluated data-deficient species
library(rredlist)
iucn_key_n <- "..."

preds$category_new<-NA
for (i in 1:nrow(preds)) {
  print(i)
  cats<-rredlist::rl_history(as.character(preds$binomial[i]), key=iucn_key_n)
  if(length(cats$result)>0){
    preds$category_new[i]<-cats$result$code[1]
  }
}
preds_upd <- subset(preds, preds$category_new!="DD")
preds_upd$category_group_new<-"threatened"
preds_upd$category_group_new[which(preds_upd$category_new %in% c("LC","NT"))] <- "not threatened"
preds_upd$category_new<-factor(preds_upd$category_new, levels = c("LC","NT","VU","EN","CR"))
preds_upd$count<-1

#save(preds_upd, file="~/GitHub/dd_forecast/dataframes/preds_dd_2021_1")
#save(preds_upd, file="~/GitHub/dd_forecast/dataframes/preds_dd_2021_2")
#load(file="~/GitHub/dd_forecast/dataframes/preds_dd_2021_1")
load(file="~/GitHub/dd_forecast/dataframes/preds_dd_2021_2")


# generat plot for overview
preds_upd_mar<-subset(preds_upd, tolower(preds_upd$marine)=="true")
preds_upd_nonmar<-subset(preds_upd, tolower(preds_upd$marine)!="true")

library(viridis)
par(mar=c(4,4,3,0.5))
par(mfrow=c(1,2))
a<-plot(factor(preds_upd_mar$category_new), preds_upd_mar$threatened,col=inferno(5), main="marine species",ylab="Predicted PE score",xlab="",cex=0.4,cex.lab=0.8,cex.axis=0.7,cex.main=0.8, ylim=c(0,1))
mtext(paste("n = ", a$n, sep=""), side=1, at=c(1:length(a$n)),cex=0.55, font=1, padj = 5) 
b<-plot(factor(preds_upd_nonmar$category_new), preds_upd_nonmar$threatened, col=inferno(5),ylab="",cex=0.4,cex.lab=0.8,cex.axis=0.7,cex.main=0.8, main="non-marine species",xlab="", ylim=c(0,1))
mtext(paste("n = ", b$n, sep=""), side=1, at=c(1:length(b$n)),cex=0.55, font=1, padj = 5) 
par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(4, 1, 1, 1), new=TRUE)
plot(factor(preds_upd_nonmar$category_new), preds_upd_nonmar$threatened, cex.lab=0.8,yaxt="n",frame=F, xaxt="n",xlab="Actual IUCN threat-level", ylim=c(0,0.01))
