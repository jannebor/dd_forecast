library(raster)
library(doParallel)
library(parallel)

# load IUCN range maps, retrieved from https://www.iucnredlist.org/resources/spatial-data-download
# for instance spatial data for amphibians
pol<-shapefile("/AMPHIBIANS.shp")

# number of cores used for parallel processing
numCores = 4

c1 <- makeCluster(numCores)
registerDoParallel(c1)
numCoresUsed <- getDoParWorkers()

df_ml_sub<-foreach(s = seq_len(length(unique(pol$id_no))), .combine = "rbind") %dopar% {
  
  sub_pol<-subset(pol, pol$id_no==unique(pol$id_no)[s])
  
  #using the function created in data_extraction.R
  if(!exists("data_extraction")) {
    
    source("~/GitHub/dd_forecast/workflow/data_extraction.R")
    
  }
  
  df_extr<-data_extraction(sub_pol)
  
  return(df_extr)  
  
}

stopCluster(c1)