#options(java.parameters = "-Xmx32000m")
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

### load data
#################
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

################

# create function
data_extraction<-function(species_polygon){
  
  #only consider native and extant species
  species_polygon<-subset(species_polygon, species_polygon$presence==1)
  species_polygon<-subset(species_polygon, species_polygon$origin==1)
  
  if(nrow(species_polygon)>0){
    
    #change crs if other than wgs84
    if(length(grep("wgs84",tolower(crs(species_polygon))))<1){
      species_polygon <- species_polygon%>% st_transform("+proj=longlat +datum=WGS84 +no_defs")
    }
    
    #create data frame
    df_ml.ii<-data.frame()
    
    #extract data for each "seasonal" range map
    for(m in 1:length(unique(species_polygon$seasonal))){

      species_polygon.i<-subset(species_polygon, species_polygon$seasonal==species_polygon$seasonal[m])
      
      if(nrow(species_polygon.i)>0){
        
        # make dataframe with species information
        df_ml.i<-as.data.frame(species_polygon.i)
        df_ml.i<-df_ml.i[-which(names(df_ml.i) %in% "geometry")]
        
        # create columns for data extraction from tables
        columns= c(
          "countries","observations","obs_cells","range_extent",
          paste(rep("aware_irr_",4),c("median","mean","min","max"),sep=""),
          paste(rep("aware_non_irr_",4),c("median","mean","min","max"),sep=""),
          paste(rep("cpi_",4),c("median","mean","min","max"),sep=""),
          paste(rep("hdi_",4),c("median","mean","min","max"),sep=""),
          paste(rep("hdi_dev_",4),c("median","mean","min","max"),sep=""),
          paste(rep("reactive_",4),c("median","mean","min","max"),sep=""),
          paste(rep("proactive_",4),c("median","mean","min","max"),sep=""),
          "CI_current","CI_future","n_powerplants","n_dams","n_habitats",
          "n_subhabitats","n_importanthabitats","perc_importanthabitats",
          paste(rep("habitat",18),c(1:18),sep="")
        ) 
        df<-data.frame(matrix(nrow=1, ncol = length(columns)))
        colnames(df) = columns
       
        # bind empty columns to species information
        df_ml.i<-cbind(df_ml.i,df)
        

        #extract data
        ######################################
        # native countries from IUCN in a while loop to avoid "BAD CONNECTION" error
        country_list<-NULL
        t0<-Sys.time()
        while(length(country_list)<1){
          tryCatch({
            country_list<-iucn_native_country(paste(species_polygon.i$binomial[1]), iucn_key_n)
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
          t1<-Sys.time()
          print((t1-t0)[[1]])
          if(length(country_list)<1){
            Sys.sleep(2)
          }
          if((t1-t0)[[1]]>30){
            break
          }
        }
        # assign number of native countries to correct column and row
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("countries"))] <- length(country_list)
        
 
        # retrieve habitats from IUCN API in while loop to avoid "BAD CONNECTION" error
        hab_ls<-NULL
        t0<-Sys.time()
        while(length(hab_ls)<1){
          tryCatch({
            hab_ls <- rl_habitats(as.character(species_polygon.i$binomial[1]), key=iucn_key_n)
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
          t1<-Sys.time()
          print((t1-t0)[[1]])
          if(length(hab_ls)<1){
            Sys.sleep(2)
          }
          if((t1-t0)[[1]]>30){
            break
          }
        }
        
        #  habitats stored in $results
        hab_ls<-hab_ls$result
        # assign data to dataframe if habitats assessed for the species
        if(length(hab_ls)>0){
          # assign number of subhabitats to correct column and row
          df_ml.i$n_subhabitats[which(df_ml.i$binomial==species_polygon.i$binomial[1])]<-length(unique(hab_ls$code))
          
          # only consider integer as main habitat type and assign number of habitats to correct column and row
          hab_ls$code<-str_sub(hab_ls$code, 1, 4)
          df_ml.i$n_habitats[which(df_ml.i$binomial==species_polygon.i$binomial[1])]<-length(unique(as.integer(hab_ls$code)))
          
          # assign number of important main habitats to correct column and row
          hab_ls$code<-as.integer(hab_ls$code)
          df_ml.i$n_importanthabitats[which(df_ml.i$binomial==species_polygon.i$binomial[1])]<-length(unique(hab_ls$code[which(hab_ls$majorimportance=="Yes")]))
          
          # assign categorical factors (yes/no)
          for(h in 1:18){
            if(h %in% hab_ls$code){
              df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]),which(names(df_ml.i) %in% paste("habitat",h,sep=""))]<-"Yes"
            } else {
              df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]),which(names(df_ml.i) %in% paste("habitat",h,sep=""))]<-"No"
            }
            
          }
          
          # calculate fraction of important habitats
          df_ml.i$perc_importanthabitats<-df_ml.i$n_importanthabitats/df_ml.i$n_habitats
        }
        
        
        # retrieve GBIF data
        key<-NULL
        occ_points<-NULL
        occ_cell<-NULL
        key<-name_backbone(name=paste(species_polygon.i$binomial[1]))$speciesKey
        if(length(key)>0){
          if(length(country_list)>0){
            
            #create empty DataFrame to store occurrence data from different native countries
            occ_points <- data.frame(x=NA,y=NA,country=NA)
            
            for (c in 1:length(country_list)){
              occ<-NULL
              # retrieve max. number of records per request from each native countrye
              tryCatch({
                occ<-occ_search(taxonKey=key, country = paste(country_list[c]), year="2010,2020", fields="all", hasCoordinate = T, hasGeospatialIssue = F,limit=100000)
              }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
              
              #bind data to dataframe
              if (!is.null(occ$data)){
                occ_add <- data.frame(x=occ$data$decimalLongitude,y=occ$data$decimalLatitude,country=country_list[c])
                occ_points<-rbind(occ_points,occ_add)
              }
            }
            
            occ_points<-na.exclude(occ_points)
            
            if(nrow(occ_points)>0){
              #convert x y to spatial points
              occ_points<-SpatialPoints(data.frame(x=occ_points$x,y=occ_points$y), CRS("+proj=longlat +datum=WGS84"))
              
              #select occurrence cells (cells that contain at least one occurrence point)
              occ_cell <- occ_cell_fishnet[!is.na(sp::over(occ_cell_fishnet, sp::geometry(occ_points))), ]

            } else {occ_points<-NULL}
          }
        }
        
        # retrieve OBIS data for marine species only
        if(species_polygon.i$marine=="true"){
          occ_ob<-NULL
          occ_points_ob<-NULL
          occ_cell_ob<-NULL

          tryCatch({
            occ_ob <- occurrence(species_polygon.i$binomial[1], startdate = "2010-01-01")
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
          
          # remove absence data
          if(nrow(occ_ob)>0){
            occ_ob<-subset(occ_ob, occ_ob$absence=="FALSE")
            if(nrow(occ_ob)>0){
              #convert x y to spatial points
              occ_points_ob<-SpatialPoints(data.frame(x=occ_ob$decimalLongitude,y=occ_ob$decimalLatitude), CRS("+proj=longlat +datum=WGS84"))
              
              #select occurrence cells (cells that contain at least one occurrence point)
              occ_cell_ob <- occ_cell_fishnet[!is.na(sp::over(occ_cell_fishnet, sp::geometry(occ_points_ob))), ]
              if(length(occ_points)>0){
                #combine to data retrieved from GBIF
                occ_points<-gUnion(occ_points, occ_points_ob)
                occ_cell<-gUnion(occ_cell, occ_cell_ob)
              } else {
                occ_points<-occ_points_ob
                occ_cell<-occ_cell_ob
              }
            }
          }
        }
        
        # assign number of occurrence points to correct column and row
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("observations"))] <- length(occ_points)
     
        # assign number of occurrence cells to correct column and row
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("obs_cells"))] <- length(occ_cell)
        
        range_ras<-NULL
        # rasterize range polygon
        range_ras<-fasterize(species_polygon.i, predictors_stack[[24]])
        range_ras[!is.na(range_ras)]<-1
        
        # assign number of range map cells as estimate of range extent to correct column and row
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("range_extent"))] <- length(range_ras[!is.na(range_ras)])
        
        # assign current and future connectivity index to correct column and row
        if(length(CI[which(CI$binomial==species_polygon.i$binomial[1]),3])>0){
          df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                  which(names(df_ml.i) %in% paste("CI_current"))]<- CI[which(CI$binomial==species_polygon.i$binomial[1]),3]
        }
        if(length(CI[which(CI$binomial==species_polygon.i$binomial[1]),4])>0){
          df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                  which(names(df_ml.i) %in% paste("CI_future"))]<- CI[which(CI$binomial==species_polygon.i$binomial[1]),4]
        }
        
        
        
        hdi_val<-c()
        hdi_dev<-c()
        cpi_val<-c()
        react_val<-c()
        proact_val<-c()
        aware_irr<-c()
        aware_non_irr<-c()
        # retrieve human development index, corruption perception index, reactive and proactive capacities to respond to biological invasions and water scarcity estimates from tables
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
        
        # assign median, mean, min and max of values retrieved from table to correct column and row
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_irr_median"))]<-median(na.exclude(as.numeric(aware_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_irr_mean"))]<-mean(na.exclude(as.numeric(aware_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_irr_min"))]<-min(na.exclude(as.numeric(aware_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_irr_max"))]<-max(na.exclude(as.numeric(aware_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_non_irr_median"))]<-median(na.exclude(as.numeric(aware_non_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_non_irr_mean"))]<-mean(na.exclude(as.numeric(aware_non_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_non_irr_min"))]<-min(na.exclude(as.numeric(aware_non_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("aware_non_irr_max"))]<-max(na.exclude(as.numeric(aware_non_irr)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("cpi_median"))]<-median(na.exclude(as.numeric(cpi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("cpi_mean"))]<-mean(na.exclude(as.numeric(cpi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("cpi_min"))]<-min(na.exclude(as.numeric(cpi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("cpi_max"))]<-max(na.exclude(as.numeric(cpi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_median"))]<-median(na.exclude(as.numeric(hdi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_mean"))]<-mean(na.exclude(as.numeric(hdi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_min"))]<-min(na.exclude(as.numeric(hdi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_max"))]<-max(na.exclude(as.numeric(hdi_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_dev_median"))]<-median(na.exclude(as.numeric(hdi_dev)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_dev_mean"))]<-mean(na.exclude(as.numeric(hdi_dev)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_dev_min"))]<-min(na.exclude(as.numeric(hdi_dev)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("hdi_dev_max"))]<-max(na.exclude(as.numeric(hdi_dev)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("reactive_median"))]<-median(na.exclude(as.numeric(react_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("reactive_mean"))]<-mean(na.exclude(as.numeric(react_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("reactive_min"))]<-min(na.exclude(as.numeric(react_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("reactive_max"))]<-max(na.exclude(as.numeric(react_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("proactive_median"))]<-median(na.exclude(as.numeric(proact_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("proactive_mean"))]<-mean(na.exclude(as.numeric(proact_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("proactive_min"))]<-min(na.exclude(as.numeric(proact_val)))
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("proactive_max"))]<-max(na.exclude(as.numeric(proact_val)))
        

        # assign number of power plants within species range to correct column and row
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("n_powerplants"))] <- length(sf::st_intersects(species_polygon.i,powerplants)[[1]])
        
        # assign number of dams within species range to correct column and row
        df_ml.i[which(df_ml.i$binomial==species_polygon.i$binomial[1]&df_ml.i$seasonal==species_polygon.i$seasonal[1]),
                which(names(df_ml.i) %in% paste("n_dams"))] <- length(sf::st_intersects(species_polygon.i,dams)[[1]])
        
      

        # extract spatial data within range (median, mean, min and max) and bind columns to dataframe
        extr_pol<-species_polygon.i
        df_ml.i<-cbind(df_ml.i,exactextractr::exact_extract(predictors_stack, extr_pol, c('median','mean','min','max')))
        
        # save variable names of spatial data to duplicate when no occurrence cells available
        r_cnams<-names(df_ml.i[86:length(df_ml.i)])
        
        if(length(occ_cell)>0){
          extr_pol<-sf::st_as_sf(aggregate(occ_cell))
          df_ml.i<-cbind(df_ml.i,exactextractr::exact_extract(predictors_stack, extr_pol, c('median','mean','min','max')))
          colnames(df_ml.i)[1642:length(df_ml.i)]<-stringr::str_c("occ_",r_cnams)
          
        } else {
          df_ml.i[,c(1642:(1641+length(r_cnams)))]<-NA
          colnames(df_ml.i)[1642:length(df_ml.i)]<-stringr::str_c("occ_",r_cnams)
        }
        
        df_ml.ii<-rbind(df_ml.ii, df_ml.i)
        
   
      }
    }
    
    # return dataframe if data extraction successful 
    if(nrow(df_ml.ii)>0){
      return(df_ml.ii)  
    }
  }
}


#example
#getting IUCN range maps
#pol<-st_read("~/GitHub/dd_forecast/files/range_maps/Version2020-3/AMPHIBIANS/AMPHIBIANS.shp")
#species_polygon<-subset(pol, pol$id_no==unique(pol$id_no)[3])
#df_extr<-data_extraction(species_polygon)
