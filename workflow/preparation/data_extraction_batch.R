library(doSNOW)
library(sf)

# requires IUCN API access
if(!exists("iucn_key_n")){
  if(file.exists("~/GitHub/dd_forecast/files/iucn_key_n")){
    load("~/GitHub/dd_forecast/files/iucn_key_n")
  } else {
    iucn_key_n <- rstudioapi::askForPassword("iucn_key_n")
  }
}


# IUCN range maps retrieved from https://www.iucnredlist.org/resources/spatial-data-download
# for example: amphibians
pol<-st_read("~/GitHub/dd_forecast/files/range_maps/Version2020-3/AMPHIBIANS/AMPHIBIANS.shp")

# number of cores used for parallel processing
numCores = 4

cl <- makeCluster(numCores)
registerDoSNOW(cl)
iterations <- length(unique(pol$id_no))
#iterations<-50
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

df_ml_sub<-foreach(s = 1:iterations, .options.snow = opts, .combine = "rbind",
                   .packages = c("raster", "stringr", "rgbif", 
                            "rgeos","rgdal",
                            "robis", "rredlist","fasterize",
                            "sf", "exactextractr")) %dopar% {
  
  sub_pol<-subset(pol, pol$id_no==unique(pol$id_no)[s])
  
  #using the function created in data_extraction.R
  if(!exists("data_extraction")) {
    
    source("~/GitHub/dd_forecast/workflow/preparation/data_extraction.R")
    
  }
  
  df_extr<-data_extraction(sub_pol)
  
  return(df_extr)  
  
}

stopCluster(cl)
