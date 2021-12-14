options(java.parameters = "-Xmx32000m")
library(rgbif)
library(raster)
library(stringr)
library(rgeos)
library(xlsx)
library(readxl)
library(robis)
library(rredlist)
library(rgdal)

### create functions
# requires IUCN API access
iucn_key_n <- "..."

iucn_native_country <-function(species, iucn_key_n){
  #iucn countries
  occ_countries<-rl_occ_country(name=species, key = iucn_key_n)$result
  occ_countries<-subset(occ_countries, occ_countries$origin=="Native")
  occ_countries<-subset(occ_countries, occ_countries$presence=="Extant")
  country_list<-paste(occ_countries$code)
  
  return(country_list)
}



# creating a fishnet in 30 arcmin resolution
occ_cell_fishnet <- raster()
res(occ_cell_fishnet) <- 0.5
crs(occ_cell_fishnet) <- CRS("+proj=longlat +datum=WGS84")
occ_cell_fishnet <- rasterToPolygons(occ_cell_fishnet)

## load raster stack: environmental stressors
# raster stack: created in mapping_raster_stack.R
load("~/GitHub/dd_forecast/files/predictors_stack")

#### the following data needs to be downloaded individually from the indicated sources:
#Boulay et al. 2018 The WULCA consensus characterization model for water scarcity footprints: assessing impacts of water consumption based on available water remaining (AWARE) doi:10.1007/s11367-017-1333-8
aware<-read.xlsx("~/GitHub/dd_forecast/files/AWARE_country_regions_Improved.xlsx", sheetIndex=1)

#Barbarossa et al. 2020 Impacts of current and future large dams on the geographic range connectivity of freshwater fish worldwide doi:10.1073/pnas.1912776117
CI<-read.xlsx("~/GitHub/dd_forecast/files/connectivity_index_pnas.xlsx", sheetIndex=1)

#Byers et al. 2019 A Global Database of Power Plants (https://datasets.wri.org/dataset/globalpowerplantdatabase)
powerplants<-read.csv("~/GitHub/dd_forecast/files/globalpowerplantdatabasev120/global_power_plant_database.csv")
powerplants<-SpatialPointsDataFrame(data.frame(x=powerplants$longitude, y=powerplants$latitude),powerplants,proj4string = CRS("+proj=longlat +datum=WGS84"))

#Mulligan et al. 2020 GOODD, a global dataset of more than 38,000 georeferenced dams doi:10.1038/s41597-020-0362-5
dams<-shapefile("~/GitHub/dd_forecast/files/GOODD_data/Data/GOOD2_dams.shp")
dams<-spTransform(dams,CRS("+proj=longlat +datum=WGS84"))

#Human development index (http://hdr.undp.org/sites/default/files/2020_statistical_annex_all.xlsx)
hdi<-read_xlsx("~/GitHub/dd_forecast/files/HDI/2020_statistical_annex_all.xlsx", sheet = 3, skip=4, n_max = 200)
hdi<-hdi[c(2,seq(3,27,2))]
names(hdi)[2:length(hdi)]<-paste("X",names(hdi)[2:length(hdi)],sep="")
names(hdi)[2:length(hdi)]<-gsub(names(hdi)[2:length(hdi)],pattern = "-",replacement = ".")
hdi[2:14]<-lapply(hdi[2:14],as.numeric)
hdi<-hdi[which(hdi$Country != toupper(hdi$Country)),]

#Corruption Perceptions Index 2020 (https://images.transparencycdn.org/images/CPI_FULL_DATA_2021-01-27-162209.zip)
cpi<-read_xlsx("~/GitHub/dd_forecast/files/CPI/CPI2020_GlobalTablesTS_210125.xlsx", sheet = 2, skip=2)
cpi<-cpi[c(1,4)]
names(cpi)[2]<-gsub(names(cpi)[2], pattern=" ", replacement = ".")

#Early et al. 2016 Global threats from invasive alien species in the twenty-first century and national response capacities (doi:10.1038/ncomms12485)
cpd<-read_xlsx("~/GitHub/dd_forecast/files/CBD/CBDreport_summary_english_spanish_french.xlsx", sheet = 1)
cpd<-cpd[c(1,11,12)]
cpd<-na.exclude(cpd)

data_extraction<-function(species_polygon){
  lapply(c("raster", "stringr", "rgbif", 
                "rgeos","rgdal",
                "robis", "rredlist",
                "sf", "exactextractr"), require, character.only=T)
                  
                               
                              
                               species_polygon<-subset(species_polygon, species_polygon$presence==1)
                               species_polygon<-subset(species_polygon, species_polygon$origin==1)
                               
                               
                               if(length(species_polygon)>0){
                                 
                                 species_polygon<-spTransform(species_polygon,CRS("+proj=longlat +datum=WGS84"))
                                 
                                 for(m in 1:length(unique(species_polygon$seasonal))){
                                   species_polygon<-subset(species_polygon, species_polygon$seasonal==species_polygon$seasonal[m])
                                   
                                   if(nrow(species_polygon)>0){
                                     
                                     df_ml.i<-species_polygon@data
                                     
                                     
                                     if(paste("countries") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("countries"))
                                     }
                                     
                                     country_list<-NULL
                                     
                                     tryCatch({
                                       country_list<-iucn_native_country(paste(species_polygon$binomial[1]), iucn_key_n)
                                     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                     
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("countries"))] <- length(country_list)
                                     
                                     
                                     
                                     # retrieve GBIF data
                                     key<-NULL
                                     occ_points<-NULL
                                     occ_cell<-NULL
                                     occ_ras<-NULL
                                     key<-name_backbone(name=paste(species_polygon$binomial[1]))$speciesKey
                                     if(length(key)>0){
                                       if(length(country_list)>0){
                                         
                                         #create empty DataFrame
                                         occ_points <- data.frame(x=NA,y=NA,country=NA)
                                         
                                         for (c in 1:length(country_list)){
                                           occ<-NULL
                                           tryCatch({
                                             occ<-occ_search(taxonKey=key, country = paste(country_list[c]), year="2010,2020", fields="all", hasCoordinate = T, hasGeospatialIssue = F,limit=100000)
                                           }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                           
                                           
                                           if (!is.null(occ$data)){
                                             occ_add <- data.frame(x=occ$data$decimalLongitude,y=occ$data$decimalLatitude,country=country_list[c])
                                             occ_points<-rbind(occ_points,occ_add)
                                             
                                           }
                                           
                                         }
                                         
                                         occ_points<-na.exclude(occ_points)
                                         if(nrow(occ_points)>0){
                                           occ_points<-SpatialPoints(data.frame(x=occ_points$x,y=occ_points$y), CRS("+proj=longlat +datum=WGS84"))
                                           occ_cell <- occ_cell_fishnet[!is.na(sp::over(occ_cell_fishnet, sp::geometry(occ_points))), ]
                                           occ_ras<-rasterize(occ_cell, predictors_stack[[24]])
                                           occ_ras[!is.na(occ_ras)]<-1
                                         } else {occ_points<-NULL}
                                         
                                         
                                       }
                                     }
                                     
                                     # retrieve OBIS data
                                     if(species_polygon$marine=="true"){
                                       occ_ob<-NULL
                                       occ_points_ob<-NULL
                                       occ_cell_ob<-NULL
                                       occ_ras_ob<-NULL
                                       
                                       
                                       tryCatch({
                                         occ_ob <- occurrence(species_polygon$binomial[1], startdate = "2010-01-01")
                                       }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                       
                                       if(length(occ_ob)>0){
                                         occ_ob<-subset(occ_ob, occ_ob$absence=="FALSE")
                                         if(nrow(occ_ob)>0){
                                           occ_ob<-subset(occ_ob, occ_ob$absence=="FALSE")
                                           if(nrow(occ_ob)>0){
                                             occ_points_ob<-SpatialPoints(data.frame(x=occ_ob$decimalLongitude,y=occ_ob$decimalLatitude), CRS("+proj=longlat +datum=WGS84"))
                                             occ_cell_ob <- occ_cell_fishnet[!is.na(sp::over(occ_cell_fishnet, sp::geometry(occ_points_ob))), ]
                                             occ_ras_ob<-rasterize(occ_cell_ob, predictors_stack[[24]])
                                             occ_ras_ob[!is.na(occ_ras_ob)]<-1
                                             if(length(occ_points)>0){
                                               occ_points<-gUnion(occ_points, occ_points_ob)
                                               occ_cell<-gUnion(occ_cell, occ_cell_ob)
                                             } else {
                                               occ_points<-occ_points_ob
                                               occ_cell<-occ_cell_ob
                                             }
                                           }
                                         }
                                       }
                                     }
                                     
                                     
                                     
                                     if(paste("observations") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("observations"))
                                     }
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("observations"))] <- length(occ_points)
                                     
                                     if(paste("obs_cells") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("obs_cells"))
                                     }
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("obs_cells"))] <- length(occ_cell)
                                     
                                     
                                     
                                     
                                     range_ras<-NULL
                                     range_ras<-rasterize(species_polygon, predictors_stack[[24]])
                                     range_ras[!is.na(range_ras)]<-1
                                     
                                     if(paste("range_extent") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("range_extent"))
                                     }
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("range_extent"))] <- length(range_ras[!is.na(range_ras)])
                                     
                                     
                                     if(paste("aware_irr_mean") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_irr_median"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_irr_mean"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_irr_min"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_irr_max"))
                                     }
                                     
                                     if(paste("aware_non_irr_mean") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_non_irr_median"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_non_irr_mean"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_non_irr_min"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("aware_non_irr_max"))
                                     }
                                     
                                     if(paste("cpi_mean") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("cpi_median"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("cpi_mean"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("cpi_min"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("cpi_max"))
                                     }
                                     
                                     if(paste("hdi_mean") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_median"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_mean"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_min"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_max"))
                                     }
                                     
                                     if(paste("hdi_dev_mean") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_dev_median"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_dev_mean"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_dev_min"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("hdi_dev_max"))
                                     }
                                     
                                     if(paste("reactive_mean") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("reactive_median"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("reactive_mean"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("reactive_min"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("reactive_max"))
                                     }
                                     if(paste("proactive_mean") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("proactive_median"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("proactive_mean"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("proactive_min"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("proactive_max"))
                                     }
                                     
                                     if(paste("CI_current") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("CI_current"))
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("CI_future"))
                                     }
                                     
                                     if(length(CI[which(CI$binomial==species_polygon$binomial[1]),3])>0){
                                       df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                               which(names(df_ml.i) %in% paste("CI_current"))]<- CI[which(CI$binomial==species_polygon$binomial[1]),3]
                                     }
                                     
                                     if(length(CI[which(CI$binomial==species_polygon$binomial[1]),4])>0){
                                       df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                               which(names(df_ml.i) %in% paste("CI_future"))]<- CI[which(CI$binomial==species_polygon$binomial[1]),4]
                                     }
                                     
                                     
                                     hdi_val<-c()
                                     hdi_dev<-c()
                                     cpi_val<-c()
                                     react_val<-c()
                                     proact_val<-c()
                                     aware_irr<-c()
                                     aware_non_irr<-c()
                                     for (c in 1:length(country_list)) {
                                       if(length(hdi[which(tolower(gsub("[(),]", "", hdi$Country))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),9])>0){
                                         hdi_val<-c(hdi_val,hdi[which(tolower(gsub("[(),]", "", hdi$Country))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),9])
                                         hdi_dev<-c(hdi_dev,hdi[which(tolower(gsub("[(),]", "", hdi$Country))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),14])
                                       }
                                       if(length(cpi[which(tolower(gsub("[(),]", "", cpi$Country))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),2])>0){
                                         cpi_val<-c(cpi_val,cpi[which(tolower(gsub("[(),]", "", cpi$Country))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),2])
                                       }
                                       if(length(aware[which(tolower(gsub("[(),]", "", aware$NA.))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),2])>0){
                                         aware_irr<-c(aware_irr,aware[which(tolower(gsub("[(),]", "", aware$NA.))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),2])
                                         aware_non_irr<-c(aware_non_irr,aware[which(tolower(gsub("[(),]", "", aware$NA.))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),3])
                                       }
                                       if(length(cpd[which(tolower(gsub("[(),]", "", cpd$Country_global))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),2])>0){
                                         react_val<-c(react_val,cpd[which(tolower(gsub("[(),]", "", cpd$Country_global))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),2])
                                         proact_val<-c(proact_val,cpd[which(tolower(gsub("[(),]", "", cpd$Country_global))==tolower(gsub("[(),]", "", isocodes[which(isocodes$code==country_list[c]),3]))),3])
                                       }
                                     }
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_irr_median"))]<-median(na.exclude(as.numeric(aware_irr)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_irr_mean"))]<-mean(na.exclude(as.numeric(aware_irr)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_irr_min"))]<-min(na.exclude(as.numeric(aware_irr)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_irr_max"))]<-max(na.exclude(as.numeric(aware_irr)))
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_non_irr_median"))]<-median(na.exclude(as.numeric(aware_non_irr)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_non_irr_mean"))]<-mean(na.exclude(as.numeric(aware_non_irr)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_non_irr_min"))]<-min(na.exclude(as.numeric(aware_non_irr)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("aware_non_irr_max"))]<-max(na.exclude(as.numeric(aware_non_irr)))
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("cpi_median"))]<-median(na.exclude(as.numeric(cpi_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("cpi_mean"))]<-mean(na.exclude(as.numeric(cpi_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("cpi_min"))]<-min(na.exclude(as.numeric(cpi_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("cpi_max"))]<-max(na.exclude(as.numeric(cpi_val)))
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_median"))]<-median(na.exclude(as.numeric(hdi_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_mean"))]<-mean(na.exclude(as.numeric(hdi_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_min"))]<-min(na.exclude(as.numeric(hdi_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_max"))]<-max(na.exclude(as.numeric(hdi_val)))
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_dev_median"))]<-median(na.exclude(as.numeric(hdi_dev)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_dev_mean"))]<-mean(na.exclude(as.numeric(hdi_dev)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_dev_min"))]<-min(na.exclude(as.numeric(hdi_dev)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("hdi_dev_max"))]<-max(na.exclude(as.numeric(hdi_dev)))
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("reactive_median"))]<-median(na.exclude(as.numeric(react_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("reactive_mean"))]<-mean(na.exclude(as.numeric(react_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("reactive_min"))]<-min(na.exclude(as.numeric(react_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("reactive_max"))]<-max(na.exclude(as.numeric(react_val)))
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("proactive_median"))]<-median(na.exclude(as.numeric(proact_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("proactive_mean"))]<-mean(na.exclude(as.numeric(proact_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("proactive_min"))]<-min(na.exclude(as.numeric(proact_val)))
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("proactive_max"))]<-max(na.exclude(as.numeric(proact_val)))
                                     
                                     
                                     
                                     
                                     if(paste("n_powerplants") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("n_powerplants"))
                                     }
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("n_powerplants"))] <- length(powerplants[!is.na(sp::over(powerplants, sp::geometry(species_polygon))), ])
                                     
                                     if(paste("n_dams") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("n_dams"))
                                     }
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("n_dams"))] <- length(dams[!is.na(sp::over(dams, sp::geometry(species_polygon))), ])
                                     
                                     if(paste("population_size") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("population_size"))
                                     }
                                     
                                     if(paste("population_trend5") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("population_trend5"))
                                     }
                                     
                                     if(paste("population_trend10") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("population_trend10"))
                                     }
                                     
                                     # estimate species population
                                     trend<-NULL
                                     occ<-NULL
                                     occ_all<-NULL
                                     occ1<-NULL
                                     all<-NULL
                                     if(length(country_list)>0){
                                       
                                       #POPULATION TREND
                                       trend<-NULL
                                       occ<-NULL
                                       occ_all<-NULL
                                       occ1<-NULL
                                       all<-NULL
                                       tryCatch({
                                         occ1<-rgbif::count_facet(key, by='country', countries=country_list, removezeros = FALSE)
                                       }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                       tryCatch({
                                         all<-rgbif::count_facet(name_backbone(name=paste(species_polygon$binomial[1]))$classKey, by='country', countries=country_list, removezeros = FALSE)
                                       }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                       
                                       occ1$V2<-all$count
                                       occ1$fraction<-occ1$count/occ1$V2
                                       if(length(country_list)>1){
                                         df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                                 which(names(df_ml.i) %in% paste("population_size"))] <-as.numeric(mean(aggregate(occ1$fraction~occ1$country, FUN=mean)[,2]))
                                       } else {
                                         df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                                 which(names(df_ml.i) %in% paste("population_size"))] <- as.numeric(occ1$fraction)
                                       }
                                       
                                       if(mean(occ1$count)>0){
                                         trend<-data.frame(fraction=NA, year=NA, country=NA, change=NA)
                                         
                                         for (c in 1:length(country_list)){
                                           
                                           for (t in 2009:2020) {
                                             
                                             occ<-NULL
                                             occ_all<-NULL
                                             tryCatch({
                                               occ<-occ_count(taxonKey=key, country = paste(country_list[c]), year=t)
                                             }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                             tryCatch({
                                               occ_all<-occ_count(taxonKey = name_backbone(name=paste(species_polygon$binomial[1]))$classKey,year=t, country = paste(country_list[c]))
                                             }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                             
                                             
                                             trend$fraction[nrow(trend)]<-occ/occ_all
                                             if(is.na(occ/occ_all)){
                                               trend$fraction[nrow(trend)]<-0
                                             }
                                             
                                             
                                             trend$year[nrow(trend)]<-t
                                             trend$country[nrow(trend)]<-country_list[c]
                                             
                                             if(nrow(trend)>1){
                                               trend$change[nrow(trend)]<-trend$fraction[nrow(trend)]-trend$fraction[(nrow(trend)-1)]
                                             }
                                             
                                             if(nrow(trend)<(length(country_list)*length(1999:2020))){
                                               trend[(nrow(trend)+1),]<-NA
                                             }
                                             
                                             
                                             
                                           }
                                         }
                                         
                                         
                                         
                                         
                                         if(length(country_list)>1){
                                           trend<-subset(trend, trend$year>2009)
                                           df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                                   which(names(df_ml.i) %in% paste("population_trend10"))] <- as.numeric(mean(aggregate(trend$change~trend$country, FUN=mean)[,2]))
                                           
                                           trend<-subset(trend, trend$year>2014)
                                           df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                                   which(names(df_ml.i) %in% paste("population_trend5"))] <- as.numeric(mean(aggregate(trend$change~trend$country, FUN=mean)[,2]))
                                         } else {
                                           trend<-subset(trend, trend$year>2009)
                                           df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                                   which(names(df_ml.i) %in% paste("population_trend10"))] <- as.numeric(mean((trend$change)))
                                           
                                           trend<-subset(trend, trend$year>2014)
                                           df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                                   which(names(df_ml.i) %in% paste("population_trend5"))] <- as.numeric(mean((trend$change)))
                                         }
                                         
                                       }
                                       
                                     }
                                     
                                     
                                     
                                     # extract raster data
                                     remove_data_spatialpolygons <- function(spdf) {
                                       SpatialPolygons(spdf@polygons, spdf@plotOrder, CRS(proj4string(spdf)))
                                     }
                                     
                                     extr_pol<-remove_data_spatialpolygons(species_polygon)
                                     extr_pol<-sf::st_as_sf(extr_pol)
                                     df_ml.i<-cbind(df_ml.i,exactextractr::exact_extract(predictors_stack, extr_pol, c('median','mean','min','max')))
                                     
                                     r_cnams<-names(df_ml.i[67:length(df_ml.i)])
                                     
                                     if(length(occ_cell)>0){
                                       extr_pol<-sf::st_as_sf(aggregate(occ_cell))
                                       df_ml.i<-cbind(df_ml.i,exactextractr::exact_extract(predictors_stack, extr_pol, c('median','mean','min','max')))
                                       colnames(df_ml.i)[1639:length(df_ml.i)]<-stringr::str_c("occ_",r_cnams)
                                       
                                     } else {
                                       df_ml.i[,c(1639:(1638+length(r_cnams)))]<-NA
                                       colnames(df_ml.i)[1639:length(df_ml.i)]<-stringr::str_c("occ_",r_cnams)
                                     }
                                     
                                     
                                     return(df_ml.i)  
                                     
                                     
                                   }
                                   
                                 }
                                 
                               }
                               
                             }


#example
#getting IUCN range maps
#pol<-shapefile("./AMPHIBIANS.shp")
#species_polygon<-subset(pol, pol$binomial==unique(pol$binomial)[1])

#df_extr<-data_extraction(species_polygon)
