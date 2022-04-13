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
library(sf)
library(fasterize)
library(exactextractr)

### create functions
# requires IUCN API access
if(!exists("iucn_key_n")){
  if(file.exists("~/GitHub/dd_forecast/files/iucn_key_n")){
    load("~/GitHub/dd_forecast/files/iucn_key_n")
  } else {
    iucn_key_n <- rstudioapi::askForPassword("iucn_key_n")
  }
}

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

## load raster stack: spatial environmental predictors
# raster stack: created in mapping_raster_stack.R
predictors_stack<-stack("~/GitHub/dd_forecast/files/raster/predictors_stack.grd")

#### the following data needs to be downloaded individually from the indicated sources:
#Boulay et al. 2018 The WULCA consensus characterization model for water scarcity footprints: assessing impacts of water consumption based on available water remaining (AWARE) doi:10.1007/s11367-017-1333-8
aware<-read.xlsx("~/GitHub/dd_forecast/files/AWARE_country_regions_Improved.xlsx", sheetIndex=1)

#Barbarossa et al. 2020 Impacts of current and future large dams on the geographic range connectivity of freshwater fish worldwide doi:10.1073/pnas.1912776117
CI<-read.xlsx("~/GitHub/dd_forecast/files/connectivity_index_pnas.xlsx", sheetIndex=1)

#Byers et al. 2019 A Global Database of Power Plants (https://datasets.wri.org/dataset/globalpowerplantdatabase)
powerplants<-read.csv("~/GitHub/dd_forecast/files/globalpowerplantdatabasev120/global_power_plant_database.csv")
powerplants <- st_as_sf(powerplants, coords = c("longitude","latitude"))
powerplants <- st_set_crs(powerplants, 4326) 

#Mulligan et al. 2020 GOODD, a global dataset of more than 38,000 georeferenced dams doi:10.1038/s41597-020-0362-5
dams<-st_read("~/GitHub/dd_forecast/files/GOODD_data/Data/GOOD2_dams.shp")
dams <- st_set_crs(dams, 4326) 

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

                               species_polygon<-subset(species_polygon, species_polygon$presence==1)
                               species_polygon<-subset(species_polygon, species_polygon$origin==1)
                               
                               
                               if(nrow(species_polygon)>0){
                                 
                                 if(length(grep("wgs84",tolower(crs(species_polygon))))<1){
                                   
                                   species_polygon <- species_polygon%>% st_transform("+proj=longlat +datum=WGS84 +no_defs")
                                   
                                 }
                                 

                                 
                                 for(m in 1:length(unique(species_polygon$seasonal))){
                                   species_polygon<-subset(species_polygon, species_polygon$seasonal==species_polygon$seasonal[m])
                                   
                                   if(nrow(species_polygon)>0){
                                     
                                     df_ml.i<-as.data.frame(species_polygon)
                                     df_ml.i<-df_ml.i[-which(names(df_ml.i) %in% "geometry")]
                                     
                                     
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
                                     range_ras<-fasterize(species_polygon, predictors_stack[[24]])
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
                                             which(names(df_ml.i) %in% paste("n_powerplants"))] <- length(sf::st_intersects(species_polygon,powerplants)[[1]])

                                     
                                     if(paste("n_dams") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("n_dams"))
                                     }
                                     
                                     df_ml.i[which(df_ml.i$binomial==species_polygon$binomial[1]&df_ml.i$seasonal==species_polygon$seasonal[1]),
                                             which(names(df_ml.i) %in% paste("n_dams"))] <- length(sf::st_intersects(species_polygon,dams)[[1]])
                                     
                                     extr_pol<-species_polygon
                                     df_ml.i<-cbind(df_ml.i,exactextractr::exact_extract(predictors_stack, extr_pol, c('median','mean','min','max')))
                                     
                                     r_cnams<-names(df_ml.i[64:length(df_ml.i)])
                                     
                                     if(length(occ_cell)>0){
                                       extr_pol<-sf::st_as_sf(aggregate(occ_cell))
                                       df_ml.i<-cbind(df_ml.i,exactextractr::exact_extract(predictors_stack, extr_pol, c('median','mean','min','max')))
                                       colnames(df_ml.i)[1620:length(df_ml.i)]<-stringr::str_c("occ_",r_cnams)
                                       
                                     } else {
                                       df_ml.i[,c(1620:(1619+length(r_cnams)))]<-NA
                                       colnames(df_ml.i)[1620:length(df_ml.i)]<-stringr::str_c("occ_",r_cnams)
                                     }
                                     
                                     
                                     
                                     hab_ls<-NULL
                                     t0<-Sys.time()
                                     while(length(hab_ls)<1){
                                       tryCatch({
                                         hab_ls <- rl_habitats(as.character(species_polygon$binomial[1]), key=iucn_key_n)
                                       }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                       t1<-Sys.time()
                                       print((t1-t0)[[1]])
                                       Sys.sleep(2)
                                       if((t1-t0)[[1]]>30){
                                         break
                                       }
                                     }
                                     
                                     
                                     
                                     hab_ls<-hab_ls$result

                                     if(paste("n_habitats") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("n_habitats"))
                                     }
                                     
                                     if(paste("n_subhabitats") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("n_subhabitats"))
                                     }
                                     
                                     if(paste("n_importanthabitats") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("n_importanthabitats"))
                                     }
                                     
                                     if(paste("perc_importanthabitats") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("perc_importanthabitats"))
                                     }
                                     
                                     if(paste("habitat1") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat1"))
                                     }
                                     if(paste("habitat2") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat2"))
                                     }
                                     if(paste("habitat3") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat3"))
                                     }
                                     if(paste("habitat4") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat4"))
                                     }
                                     if(paste("habitat5") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat5"))
                                     }
                                     if(paste("habitat6") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat6"))
                                     }
                                     if(paste("habitat7") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat7"))
                                     }
                                     if(paste("habitat8") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat8"))
                                     }
                                     if(paste("habitat9") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat9"))
                                     }
                                     if(paste("habitat10") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat10"))
                                     }
                                     if(paste("habitat11") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat11"))
                                     }
                                     if(paste("habitat12") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat12"))
                                     }
                                     if(paste("habitat13") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat13"))
                                     }
                                     if(paste("habitat14") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat14"))
                                     }
                                     if(paste("habitat15") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat15"))
                                     }
                                     if(paste("habitat16") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat16"))
                                     }
                                     if(paste("habitat17") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat17"))
                                     }
                                     if(paste("habitat18") %in% names(df_ml.i)==FALSE){
                                       df_ml.i[ncol(df_ml.i)+1]<-NA
                                       names(df_ml.i)<-c(names(df_ml.i[-ncol(df_ml.i)]),paste("habitat18"))
                                     }
                                   
                                     
                                     if(length(hab_ls)>0){
                                       
                                       
                                       df_ml.i$n_subhabitats[which(df_ml.i$binomial==species_polygon$binomial[1])]<-length(unique(hab_ls$code))
                                       
                                       hab_ls$code<-str_sub(hab_ls$code, 1, 4)
                                       df_ml.i$n_habitats[which(df_ml.i$binomial==species_polygon$binomial[1])]<-length(unique(as.integer(hab_ls$code)))
                                       
                                       
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       
                                       df_ml.i$n_importanthabitats[which(df_ml.i$binomial==species_polygon$binomial[1])]<-length(unique(hab_ls$code[which(hab_ls$majorimportance=="Yes")]))
                                       
                                       
                                       if(1 %in% hab_ls$code){
                                         df_ml.i$habitat1[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat1[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(2 %in% hab_ls$code){
                                         df_ml.i$habitat2[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat2[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(3 %in% hab_ls$code){
                                         df_ml.i$habitat3[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat3[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(4 %in% hab_ls$code){
                                         df_ml.i$habitat4[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat4[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(5 %in% hab_ls$code){
                                         df_ml.i$habitat5[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat5[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(6 %in% hab_ls$code){
                                         df_ml.i$habitat6[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat6[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(7 %in% hab_ls$code){
                                         df_ml.i$habitat7[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat7[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(8 %in% hab_ls$code){
                                         df_ml.i$habitat8[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat8[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(9 %in% hab_ls$code){
                                         df_ml.i$habitat9[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat9[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(10 %in% hab_ls$code){
                                         df_ml.i$habitat10[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat10[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(11 %in% hab_ls$code){
                                         df_ml.i$habitat11[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat11[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(12 %in% hab_ls$code){
                                         df_ml.i$habitat12[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat12[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(13 %in% hab_ls$code){
                                         df_ml.i$habitat13[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat13[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(14 %in% hab_ls$code){
                                         df_ml.i$habitat14[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat14[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(15 %in% hab_ls$code){
                                         df_ml.i$habitat15[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat15[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(16 %in% hab_ls$code){
                                         df_ml.i$habitat16[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat16[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(17 %in% hab_ls$code){
                                         df_ml.i$habitat17[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat17[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       hab_ls$code<-as.integer(hab_ls$code)
                                       if(18 %in% hab_ls$code){
                                         df_ml.i$habitat18[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"Yes"
                                       } else {
                                         df_ml.i$habitat18[which(df_ml.i$binomial==species_polygon$binomial[1])]<-"No"
                                       }
                                       
                                       
                                       df_ml.i$perc_importanthabitats<-df_ml.i$n_importanthabitats/df_ml.i$n_habitats
                                       
                                     }
                                     
                                     
                                              
                                     
                                     return(df_ml.i)  
                                     
                                     
                                   }
                                   
                                 }
                                 
                               }
                               
                             }


#example
#getting IUCN range maps
#pol<-st_read("~/GitHub/dd_forecast/files/range_maps/Version2020-3/AMPHIBIANS/AMPHIBIANS.shp")
#species_polygon<-subset(pol, pol$id_no==unique(pol$id_no)[3])
#df_extr<-data_extraction(species_polygon)
