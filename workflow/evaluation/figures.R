library(h2o)
load(file="~/GitHub/dd_forecast/dataframes/df_ml1_v2")
# only select confirmed variables
load("~/GitHub/dd_forecast/dataframes/Partition1/variable_selection/VarSel")
df_ml1<-df_ml1[which(names(df_ml1) %in% c(names(df_ml1)[c(2,7,16:25,3233:3236)],names(VarSel$finalDecision)[which(VarSel$finalDecision=="Confirmed")]))]

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")


load("~/GitHub/dd_forecast/classifier/v2/Partition1/leaderboard")
head(leaderboard)

# import best performing model
classifier <- h2o.loadModel("classifier/v2/Partition1/h2o/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233")


# create frame for generating predictions
df_ml1_h2o<-as.h2o(df_ml1)

preds_all <- h2o.predict(classifier, df_ml1_h2o)
preds_all <- cbind(df_ml1[c(1:12,280:283)], as.data.frame(preds_all))



library(sf)

nam<-list.dirs("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Reference/Version2020-3/", recursive = F, full.names = F)
dirs<-list.dirs("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Reference/Version2020-3/", recursive = F, full.names = T)

for(d in 1:length(dirs)){
  
  shp_files<-list.files(paste(dirs[d],sep=""), pattern = ".shp$", recursive = T, full.names = T)
  for(a in 1:length(shp_files)){
    
    shapes <- st_read(shp_files[a])
    
    shapes$predict<-NA
    shapes$not_threatened<-NA
    shapes$threatened<-NA
    
    for(i in 1:length(unique(shapes$binomial))){
      
      if(length(which(preds_all$binomial==unique(shapes$binomial)[i]))>0){
        shapes$predict[which(shapes$binomial==unique(shapes$binomial)[i])]<-preds_all$predict[which(preds_all$binomial==unique(shapes$binomial)[i])]
        shapes$not_threatened[which(shapes$binomial==unique(shapes$binomial)[i])]<-preds_all$not.threatened[which(preds_all$binomial==unique(shapes$binomial)[i])]
        shapes$threatened[which(shapes$binomial==unique(shapes$binomial)[i])]<-preds_all$threatened[which(preds_all$binomial==unique(shapes$binomial)[i])]
      }
      
    
      print(i)
    }

          save(shapes, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/",nam[d],a,"_shapes",sep=""))

  }
  
}


library(raster)
library(terra)
library(sf)
library(fasterize)
ras<-raster()
ras<-terra::disaggregate(ras, 10)
ras[is.na(ras)]<-0
ras[!is.na(ras)]<-0
ras<-extend(ras, extent(-180,180,-90,90))
ras<-crop(ras, extent(-180,180,-90,90))
names(ras)<-"value"
ras


mar_all_n<-ras
mar_all_risk<-ras
mar_all_threatened<-ras
terr_all_n<-ras
terr_all_risk<-ras
terr_all_threatened<-ras

terr_dd_risk<-ras
terr_dd_n<-ras
terr_dd_threatened<-ras
mar_dd_risk<-ras
mar_dd_n<-ras
mar_dd_threatened<-ras

for(d in 1:length(dirs)){
  
  shp_files<-list.files(paste(dirs[d],sep=""), pattern = ".shp$", recursive = T, full.names = T)
  for(a in 1:length(shp_files)){
    
    load(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/",nam[d],a,"_shapes",sep=""))
    
    shapes<-subset(shapes, shapes$presence==1)
    shapes<-subset(shapes, shapes$origin==1)
    shapes<-subset(shapes, shapes$seasonal==1)
    
    shapes<-subset(shapes, shapes$category!="EX")
    shapes<-subset(shapes, shapes$category!="EW")
    
    if(nrow(shapes)>0){
      
      shapes$count<-0
      shapes$count[which(tolower(shapes$marine)=="true")]<-1
      mar_all_n_s <- fasterize(shapes, ras, field = "count", fun = "sum")
      mar_all_n_s[is.na(mar_all_n_s)]<-0
      mar_all_n<-mar_all_n+mar_all_n_s
      
      shapes$count<-shapes$threatened
      shapes$count[which(tolower(shapes$marine)!="true")]<-0
      mar_all_risk_s <- fasterize(shapes, ras, field = "count", fun = "sum")
      mar_all_risk_s[is.na(mar_all_risk_s)]<-0
      mar_all_risk<-mar_all_risk+mar_all_risk_s
      
      shapes$count<-0
      shapes$count[which(tolower(shapes$marine)=="true" & shapes$predict==2)]<-1
      mar_all_threatened_s <- fasterize(shapes, ras, field = "count", fun = "sum")
      mar_all_threatened_s[is.na(mar_all_threatened_s)]<-0
      mar_all_threatened<-mar_all_threatened+mar_all_threatened_s
      
      shapes$count<-0
      shapes$count[which(tolower(shapes$marine)!="true")]<-1
      terr_all_n_s <- fasterize(shapes, ras, field = "count", fun = "sum")
      terr_all_n_s[is.na(terr_all_n_s)]<-0
      terr_all_n<-terr_all_n+terr_all_n_s
      
      
      shapes$count<-shapes$threatened
      shapes$count[which(tolower(shapes$marine)=="true")]<-0
      terr_all_risk_s <- fasterize(shapes, ras, field = "count", fun = "sum")
      terr_all_risk_s[is.na(terr_all_risk_s)]<-0
      terr_all_risk<-terr_all_risk+terr_all_risk_s
      
      shapes$count<-0
      shapes$count[which(tolower(shapes$marine)!="true" & shapes$predict==2)]<-1
      terr_all_threatened_s <- fasterize(shapes, ras, field = "count", fun = "sum")
      terr_all_threatened_s[is.na(terr_all_threatened_s)]<-0
      terr_all_threatened<-terr_all_threatened+terr_all_threatened_s
      
      
      shapes<-subset(shapes, shapes$category=="DD")
      
      if(nrow(shapes)>0){
        shapes$count<-0
        shapes$count[which(tolower(shapes$marine)=="true")]<-1
        mar_dd_n_s <- fasterize(shapes, ras, field = "count", fun = "sum")
        mar_dd_n_s[is.na(mar_dd_n_s)]<-0
        mar_dd_n<-mar_dd_n+mar_dd_n_s
        
        shapes$count<-shapes$threatened
        shapes$count[which(tolower(shapes$marine)!="true")]<-0
        mar_dd_risk_s <- fasterize(shapes, ras, field = "count", fun = "sum")
        mar_dd_risk_s[is.na(mar_dd_risk_s)]<-0
        mar_dd_risk<-mar_dd_risk+mar_dd_risk_s
        
        shapes$count<-0
        shapes$count[which(tolower(shapes$marine)=="true" & shapes$predict==2)]<-1
        mar_dd_threatened_s <- fasterize(shapes, ras, field = "count", fun = "sum")
        mar_dd_threatened_s[is.na(mar_dd_threatened_s)]<-0
        mar_dd_threatened<-mar_dd_threatened+mar_dd_threatened_s
        
        
        shapes$count<-0
        shapes$count[which(tolower(shapes$marine)!="true")]<-1
        terr_dd_n_s <- fasterize(shapes, ras, field = "count", fun = "sum")
        terr_dd_n_s[is.na(terr_dd_n_s)]<-0
        terr_dd_n<-terr_dd_n+terr_dd_n_s
        
        
        shapes$count<-shapes$threatened
        shapes$count[which(tolower(shapes$marine)=="true")]<-0
        terr_dd_risk_s <- fasterize(shapes, ras, field = "count", fun = "sum")
        terr_dd_risk_s[is.na(terr_dd_risk_s)]<-0
        terr_dd_risk<-terr_dd_risk+terr_dd_risk_s
        
        shapes$count<-0
        shapes$count[which(tolower(shapes$marine)!="true" & shapes$predict==2)]<-1
        terr_dd_threatened_s <- fasterize(shapes, ras, field = "count", fun = "sum")
        terr_dd_threatened_s[is.na(terr_dd_threatened_s)]<-0
        terr_dd_threatened<-terr_dd_threatened+terr_dd_threatened_s
        
        
        
        
      }
      
 
    }
    
    
  }
  print(d)
}

save(mar_dd_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_n01d",sep=""))
save(mar_dd_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_risk01d",sep=""))
save(terr_dd_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_n01d",sep=""))
save(terr_dd_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_risk01d",sep=""))
save(mar_all_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_n01d",sep=""))
save(mar_all_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_risk01d",sep=""))
save(terr_all_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_n01d",sep=""))
save(terr_all_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_risk01d",sep=""))
save(mar_dd_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_threatened01d",sep=""))
save(mar_all_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_threatened01d",sep=""))
save(terr_dd_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_threatened01d",sep=""))
save(terr_all_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_threatened01d",sep=""))

############################################################

load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_risk",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_risk",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_risk",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_risk",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_threatened",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_threatened",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_threatened",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_threatened",sep=""))



########################################################
## load background images
library(viridis)
library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(raster)
library(rasterVis)
library(classInt)
library(RColorBrewer)
# Natural Earth shape files -- global (Robinson) projections
# get shapefiles from http://www.naturalearthdata.com



shape_path <- "C:/Users/janbor/Desktop/OneDrive - NTNU/Data/natural_earth_vector/"


coast_shapefile <- paste(shape_path, "50m_physical/ne_50m_coastline.shp", sep="")
ocean_shapefile <- paste(shape_path, "50m_physical/ne_50m_ocean.shp", sep="")
admin0_shapefile <- paste(shape_path, "50m_cultural/ne_50m_admin_0_countries.shp", sep="")
admin1_shapefile <- paste(shape_path, "50m_cultural/ne_50m_admin_1_states_provinces_lakes.shp", sep="")
lakes_shapefile <- paste(shape_path, "50m_physical/ne_50m_lakes.shp", sep="")
bb_shapefile <- paste(shape_path, "50m_physical/ne_50m_graticules_all/ne_50m_wgs84_bounding_box.shp", sep="")
grat30_shapefile <- paste(shape_path, "50m_physical/ne_50m_graticules_all/ne_50m_graticules_30.shp", sep="")


# find out kind of shapefile (lines vs. polygons)
layer <- ogrListLayers(coast_shapefile)
# read the shape file
coast_lines <- readOGR(coast_shapefile, layer=layer)

unproj_proj4string <- proj4string(coast_lines)
unproj_proj4string


# find out kind of shapefile (lines vs. polygons)
layer <- ogrListLayers(ocean_shapefile)
# read the shape file
ocean_poly <- readOGR(ocean_shapefile, layer=layer)

layer <- ogrListLayers(admin0_shapefile)
admin0_poly <- readOGR(admin0_shapefile, layer=layer)

layer <- ogrListLayers(admin1_shapefile)
admin1_poly <- readOGR(admin1_shapefile, layer=layer)

layer <- ogrListLayers(lakes_shapefile)
lakes_poly <- readOGR(lakes_shapefile, layer=layer)

lakes_poly$scalerank <- as.numeric(lakes_poly$scalerank)
lrglakes_poly <- subset(lakes_poly, lakes_poly$scalerank <= 2)

layer <- ogrListLayers(grat30_shapefile)
grat30_lines <- readOGR(grat30_shapefile, layer=layer)

layer <- ogrListLayers(bb_shapefile)
bb_poly <- readOGR(bb_shapefile, layer=layer)
bb_lines <- as(bb_poly, "SpatialLines")


# set Robinson CRS
unproj_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
robin_crs <- CRS("+proj=robin +lon_0=0w")
# define projections
mollCRS <- CRS('+proj=moll')
behrmannCRS <- CRS('+proj=cea +lat_ts=30')


#kaspian see missing
caspian_bb <- as(extent(45, 56, 35, 50), "SpatialPolygons")
proj4string(caspian_bb) <- unproj_proj4string
caspian_poly <- gIntersection(ocean_poly, caspian_bb)
proj4string(caspian_poly) <- unproj_proj4string
caspian_poly_proj <- spTransform(caspian_poly, robin_crs)

# do other projections
bb_poly_proj <- spTransform(bb_poly, robin_crs)
coast_lines_proj <- spTransform(coast_lines, robin_crs)
admin0_poly_proj <- spTransform(admin0_poly, robin_crs)
admin1_poly_proj <- spTransform(admin1_poly, robin_crs)
lakes_poly_proj <- spTransform(lakes_poly, robin_crs)
grat30_lines_proj <- spTransform(grat30_lines, robin_crs)
lrglakes_poly_proj <- spTransform(lrglakes_poly, robin_crs)
ocean_poly_proj <- spTransform(ocean_poly, robin_crs)

grat30_lines_proj_ocean<-erase(grat30_lines_proj, admin0_poly_proj)


# convert polygons to spatial lines
admin0_lines_proj <- as(admin0_poly_proj, "SpatialLines")
bb_lines_proj <- as(bb_poly_proj, "SpatialLines")

# set Robinson CRS
unproj_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
robin_crs <- CRS("+proj=robin +lon_0=0w")
# define projections
mollCRS <- CRS('+proj=moll')
behrmannCRS <- CRS('+proj=cea +lat_ts=30')



#Figure 1
################################################
dd_n<-terr_dd_n+mar_dd_n
dd_threatened<-terr_dd_threatened+mar_dd_threatened
plot(dd_threatened/dd_n, col=inferno(64))
#dd_threatened[dd_n<5]<-0
sst_longlat<-dd_threatened/dd_n
sst_longlat[is.na(sst_longlat)]<-0
sst_longlat[sst_longlat>0.5]<-0.5

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=1)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/output_dd_threatened.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))

plot(bb_poly_proj, col="gray20", bor="black", lwd=0.5)
#plot(grat30_lines_proj, col="gray25",add=T, lwd=0.4)
plot((sst_rob), col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot((sst_rob), legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.5,0.1),
                    labels=(c("0%","10%","20%","30%","40%","> 50%")), 
                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.4)
#plot(admin0_poly_proj, col="gray15", bor="gray20", lwd=0.4, add=TRUE)
#plot(lrglakes_poly_proj, col=cividis(9, alpha = 0.25)[5], bor=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
#plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(admin0_lines_proj, col="gray20", lwd=0.4, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Fraction of data-deficient species predicted to be threatened by extinction", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######

##############################################################



# plot and save
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/figure1_sep.png"
png(pngfile, width=16, height=15, units = "cm", res=900)


opar<-par()
par(mfrow = c(2,1),
    oma = c(0,0,0,0),
    mar = c(0,0,0,4),
    xpd=T)

#mar_dd_threatened[mar_dd_n<3]<-NA
sst_longlat<-mar_dd_threatened/mar_dd_n
#sst_longlat[is.na(sst_longlat)]<-0
sst_longlat[sst_longlat>0.5]<-0.5

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="gray10", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
plot(admin0_poly_proj, col="gray15", bor="gray20", lwd=0.5, add=TRUE)
plot(lrglakes_poly_proj, col="gray10", bor="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
#plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
#     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
#     axis.args=list(at=seq(-0.2,0.2,0.1),
#                    labels=c("< -20%","-10%",
#                             "0%","10%","> 20%"), 
#                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
plot(admin0_lines_proj, col="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
#plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
#plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("A", side=3, adj = 0.01, padj = 2, cex=1)


#terr_dd_threatened[terr_dd_n<3]<-NA
sst_longlat<-terr_dd_threatened/terr_dd_n
#sst_longlat[is.na(sst_longlat)]<-0
sst_longlat[sst_longlat>0.5]<-0.5

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="gray10", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
plot(admin0_poly_proj, col="gray15", bor="gray20", lwd=0.5, add=TRUE)
plot(lrglakes_poly_proj, col="gray10", bor="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
#plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
#     horizontal=TRUE, smallplot=c(0.1,0.9, 0.14,0.16), par(mar = par("mar")),xpd=T,
#     axis.args=list(at=seq(-0.2,0.2,0.1),
#                    labels=c("< -20%","-10%",
#                             "0%","10%","> 20%"), 
#                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
plot(admin0_lines_proj, col="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
#plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
#plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("B", side=3, adj = 0.01, padj = 2, cex=1)

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.5,
     horizontal=F, smallplot=c(0.9,0.92, 0.25,0.75), par(mar = par("mar")),outer=T,xpd=T,
     axis.args=list(at=seq(0,0.5,0.1),
                    labels=c("0%","10%",
                             "20%","30%","40%","> 50%"), 
                    cex.axis=0.5))


par(mfrow=c(1,1))

par(opar)
dev.off()
######
######################################################################




#Figure 2
#############################################################

darkcols1<-c(rev(c(
  "#000000",
  "#0584B3",
  "#0FCBFA",
  "#AFEEFD",
  "#DAF6FC"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]

darkcols1[6]<-"#A9374A"

colfunc1<-colorRampPalette(darkcols1, bias=1)


# plot and save
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/output_incrdecr_sep.png"
png(pngfile, width=16, height=15, units = "cm", res=900)


opar<-par()
par(mfrow = c(2,1),
    oma = c(0,0,0,0),
    mar = c(0,0,0,4),
    xpd=T)


sst_longlat<-(mar_all_risk)/(mar_all_n)-(mar_all_risk-mar_dd_risk)/(mar_all_n-mar_dd_n)
sst_longlat<-sst_longlat/((mar_all_risk-mar_dd_risk)/(mar_all_n-mar_dd_n))
#sst_longlat[is.na(sst_longlat)]<-0
#sst_longlat[sst_longlat==0]<-NA
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[sst_longlat<(-0.2)]<-(-0.2)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#sst_rob<-mask(sst_rob, ocean_poly_proj)

plot(bb_poly_proj, col="gray10", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
plot(admin0_poly_proj, col="gray15", bor="gray20", lwd=0.5, add=TRUE)
plot(lrglakes_poly_proj, col="gray10", bor="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(-0.2,0.2))
#plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
#     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
#     axis.args=list(at=seq(-0.2,0.2,0.1),
#                    labels=c("< -20%","-10%",
#                             "0%","10%","> 20%"), 
#                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
plot(admin0_lines_proj, col="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.3, add=TRUE)
#plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
#plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("A", side=3, adj = 0.01, padj = 2, cex=1)



sst_longlat<-(terr_all_risk)/(terr_all_n)-(terr_all_risk-terr_dd_risk)/(terr_all_n-terr_dd_n)
sst_longlat<-sst_longlat/((terr_all_risk-terr_dd_risk)/(terr_all_n-terr_dd_n))
#sst_longlat[is.na(sst_longlat)]<-0
#sst_longlat[sst_longlat==0]<-NA
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[sst_longlat<(-0.2)]<-(-0.2)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="gray10", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
plot(admin0_poly_proj, col="gray15", bor="gray20", lwd=0.5, add=TRUE)
plot(lrglakes_poly_proj, col="gray10", bor="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray40", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(-0.2,0.2))
#plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
#     horizontal=TRUE, smallplot=c(0.1,0.9, 0.14,0.16), par(mar = par("mar")),xpd=T,
#     axis.args=list(at=seq(-0.2,0.2,0.1),
#                    labels=c("< -20%","-10%",
#                             "0%","10%","> 20%"), 
#                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
plot(admin0_lines_proj, col="gray20", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray40", lwd=0.3, add=TRUE)
#plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
#plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("B", side=3, adj = 0.01, padj = 2, cex=1)

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.5, zlim=c(-0.2,0.2),
     horizontal=F, smallplot=c(0.9,0.92, 0.25,0.75), par(mar = par("mar")),outer=T,xpd=T,
     axis.args=list(at=seq(-0.2,0.2,0.1),
                    labels=c("< -20%","-10%",
                             "0%","10%","> 20%"), 
                    cex.axis=0.5))


par(mfrow=c(1,1))

par(opar)
dev.off()
##############################################



#Extended data figure 2
#######################################################
library(h2o)
load(file="~/GitHub/dd_forecast/dataframes/df_ml1_v2")
# only select confirmed variables
load("~/GitHub/dd_forecast/dataframes/Partition1/variable_selection/VarSel")
df_ml1<-df_ml1[which(names(df_ml1) %in% c(names(df_ml1)[c(2,7,16:25,3233:3236)],names(VarSel$finalDecision)[which(VarSel$finalDecision=="Confirmed")]))]

# initialize h2o, using 4 threads and 30GB memory
h2o.init(nthreads=4, max_mem_size="30g")

load("~/GitHub/dd_forecast/classifier/v2/Partition1/leaderboard")
head(leaderboard)

# import best performing model
classifier <- h2o.loadModel("classifier/v2/Partition1/h2o/StackedEnsemble_AllModels_3_AutoML_1_20220406_195233")

# create frame for generating predictions
df_ml1_h2o<-as.h2o(df_ml1)

preds_all <- h2o.predict(classifier, df_ml1_h2o)
preds_all <- cbind(df_ml1[c(1:12,280:283)], as.data.frame(preds_all))


preds_all$class<-tolower(preds_all$class)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
preds_all$class<-firstup(preds_all$class)

preds_all<-subset(preds_all, preds_all$category!="EX")
preds_all<-subset(preds_all, preds_all$category!="EW")

preds_all$count<-1
preds_all$category[which(preds_all$category=="LR/lc")]<-"LC"
preds_all$category[which(preds_all$category=="LR/cd")]<-"LC"

preds_all$category<-factor(preds_all$category, levels=c("LC","NT","VU",
                                                        "EN","CR","DD"))

preds_all$category_group<-factor(preds_all$category_group, levels=c("not threatened","threatened",
                                                                    "DD"),
                                 labels=c("not threatened","threatened","data-deficient"))

preds_all$class<-factor(preds_all$class, levels = sort(unique(preds_all$class),decreasing = F),labels = sort(unique(preds_all$class),decreasing = F))



preds_all<-subset(preds_all, tolower(preds_all$class)!=tolower("AGARICOMYCETES"))
preds_all<-subset(preds_all, tolower(preds_all$class)!=tolower("JUNGERMANNIOPSIDA"))
preds_all<-subset(preds_all, tolower(preds_all$class)!=tolower("LECANOROMYCETES"))

preds_all$class<-factor(preds_all$class, levels = sort(unique(preds_all$class),decreasing = T),labels = sort(unique(preds_all$class),decreasing = T))

#preds_all$class<-factor(preds_all$class, levels = sorted_dd$`preds_all_dd$class`,labels = sorted_dd$`preds_all_dd$class`)

library(ggridges)
library(ggplot2)
library(viridis)

p<-ggplot(preds_all, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges(panel_scaling = F, scale=1) +
  scale_fill_viridis_d(2, alpha = 0.8, direction = 1, option = "C") +
  xlab("Predicted probability of being threatened by extinction") +
  ylab("Taxonomic group") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p


pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig2.png"
png(pngfile, width=16*1.5, height=20*1.5, units = "cm", res=900)
par(mar=c(4,4,3,0.5))
par(mfrow=c(1,1))
p
dev.off()
par(mfrow=c(1,1))

###################################################################
###


###per taxonomic class: 26.10.2021
##############################
dd_dat<-cbind(preds_all)
dd_dat<-subset(dd_dat, dd_dat$category!="EX")
dd_dat<-subset(dd_dat, dd_dat$category!="EW")


udirs<-list.dirs("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Reference/Version2020-3/", recursive = F, full.names = T)
bnamlist<-c()
specieslist<-c()
#par(mfrow=c(1,1))

for(x in 1:length(udirs)){
  dirs<-list.dirs(udirs[x])
  
  if(length(dirs)>1){
    dirs<-dirs[2:length(dirs)]
  }
  
  
  
  
  for(d in 1:length(dirs)){
    
    
    shp_files<-list.files(paste(dirs[d],sep=""), pattern = ".shp$", recursive = T, full.names = T)
    
    for(a in 1:length(shp_files)){
      
      shapes <- st_read(shp_files[a])
      shapes<-subset(shapes, shapes$presence==1)
      shapes<-subset(shapes, shapes$origin==1)
      shapes<-subset(shapes, shapes$seasonal==1)
      shapes<-subset(shapes, !is.na(shapes$class))
      
      
      shapes$risk<-0
      shapes$threatened<-0
      shapes$threatened_n<-0
      shapes$count<-0
      
      for(i in 1:length(unique(shapes$binomial))){
        
        species<-unique(shapes$binomial)[i]
        
        if(unique(shapes$binomial)[i] %in% dd_dat$binomial){
          
          if(species %in% specieslist == FALSE){
            specieslist<-c(specieslist, species)
            
            
            shapes$count[which(shapes$binomial==unique(shapes$binomial)[i])]<-1
            
            if(dd_dat$predict[which(dd_dat$binomial==unique(shapes$binomial)[i])]=="threatened"){
              shapes$threatened[which(shapes$binomial==unique(shapes$binomial)[i])]<-1
            }
            
            if(dd_dat$category_group[which(dd_dat$binomial==unique(shapes$binomial)[i])]=="threatened"){
              shapes$threatened_n[which(shapes$binomial==unique(shapes$binomial)[i])]<-1
            }
            
            
            
            shapes$risk[which(shapes$binomial==unique(shapes$binomial)[i])]<-dd_dat$threatened[which(dd_dat$binomial==unique(shapes$binomial)[i])]
          }
          print(i)
          
        }
      }
      
      
      for (c in 1:length(unique(shapes$class))) {
        bnam<-unique(shapes$class)[c]
        bnamlist<-c(bnamlist, bnam)
        bnamlist<-unique(bnamlist)
        all_risk<-NULL
        all_n<-NULL
        all_threatened<-NULL
        all_threatened_n<-NULL
        
        q_select<-NULL
        q_threatened<-NULL
        q_threatened_n<-NULL
        q_all<-NULL
        
        shapes_sub<-subset(shapes, shapes$class==unique(shapes$class)[c])
        if(length(unique(shapes_sub$binomial))>0){
          
          
          q_all<-fasterize(shapes_sub, raster=ras, field = "count", fun="sum")
          q_threatened<-fasterize(shapes_sub, raster=ras, field = "threatened", fun="sum")
          q_threatened_n<-fasterize(shapes_sub, raster=ras, field = "threatened_n", fun="sum")
          q_select<-fasterize(shapes_sub, raster=ras, field = "risk", fun="sum")
          
          q_all[is.na(q_all)]<-0
          q_threatened[is.na(q_threatened)]<-0
          q_threatened_n[is.na(q_threatened_n)]<-0
          q_select[is.na(q_select)]<-0
          
          if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_risk",unique(shapes$class)[c],sep=""))){
            
            load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_risk",unique(shapes$class)[c],sep=""))
            load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",unique(shapes$class)[c],sep=""))
            load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_threatened",unique(shapes$class)[c],sep=""))
            load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_threatened_n",unique(shapes$class)[c],sep=""))
            
            all_risk <- all_risk + q_select
            all_n <- all_n + q_all
            all_threatened <- all_threatened + q_threatened
            all_threatened_n <- all_threatened_n + q_threatened_n
            
            save(all_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_risk",unique(shapes$class)[c],sep=""))
            save(all_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",unique(shapes$class)[c],sep=""))
            save(all_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_threatened",unique(shapes$class)[c],sep=""))
            save(all_threatened_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_threatened_n",unique(shapes$class)[c],sep=""))
            
            
          } else {
            
            assign("all_risk",q_select)
            assign("all_n",q_all)
            assign("all_threatened",q_threatened)
            assign("all_threatened_n",q_threatened_n)
            
            save(all_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_risk",unique(shapes$class)[c],sep=""))
            save(all_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",unique(shapes$class)[c],sep=""))
            save(all_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_threatened",unique(shapes$class)[c],sep=""))
            save(all_threatened_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_threatened_n",unique(shapes$class)[c],sep=""))
            
            
            
          }
          
          
          
          
          shapes_sub<-subset(shapes_sub, shapes_sub$category=="DD")
          if(length(unique(shapes_sub$binomial))>0){
            
            
            dd_risk<-NULL
            dd_n<-NULL
            dd_threatened<-NULL
            dd_threatened_n<-NULL
            
            q_select<-NULL
            q_threatened<-NULL
            q_threatened_n<-NULL
            q_all<-NULL
            
            
            q_all<-fasterize(shapes_sub, raster=ras, field = "count", fun="sum")
            q_threatened<-fasterize(shapes_sub, raster=ras, field = "threatened", fun="sum")
            q_threatened_n<-fasterize(shapes_sub, raster=ras, field = "threatened_n", fun="sum")
            q_select<-fasterize(shapes_sub, raster=ras, field = "risk", fun="sum")
            
            q_all[is.na(q_all)]<-0
            q_threatened[is.na(q_threatened)]<-0
            q_threatened_n[is.na(q_threatened_n)]<-0
            q_select[is.na(q_select)]<-0
            
            if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",unique(shapes$class)[c],sep=""))){
              
              load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",unique(shapes$class)[c],sep=""))
              load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",unique(shapes$class)[c],sep=""))
              load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",unique(shapes$class)[c],sep=""))
              load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened_n",unique(shapes$class)[c],sep=""))
              
              dd_risk <- dd_risk + q_select
              dd_n <- dd_n + q_all
              dd_threatened <- dd_threatened + q_threatened
              dd_threatened_n <- dd_threatened_n + q_threatened_n
              
              save(dd_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",unique(shapes$class)[c],sep=""))
              save(dd_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",unique(shapes$class)[c],sep=""))
              save(dd_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",unique(shapes$class)[c],sep=""))
              save(dd_threatened_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened_n",unique(shapes$class)[c],sep=""))
              
              
            } else {
              
              assign("dd_risk",q_select)
              assign("dd_n",q_all)
              assign("dd_threatened",q_threatened)
              assign("dd_threatened_n",q_threatened_n)
              
              save(dd_risk, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",unique(shapes$class)[c],sep=""))
              save(dd_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",unique(shapes$class)[c],sep=""))
              save(dd_threatened, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",unique(shapes$class)[c],sep=""))
              save(dd_threatened_n, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened_n",unique(shapes$class)[c],sep=""))
              
              
              
            }
            
            
            
          }
          
          
        }
      }
    }
  }
}





######################################################################################

bnamlist<-na.exclude(bnamlist)
bnamlist<-unique(bnamlist)
save(bnamlist, file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/bnamlist3",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/bnamlist3",sep=""))




#extended data figure 3:
#######################################################
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/bnamlist3",sep=""))
ras<-raster()
ras<-terra::disaggregate(ras, 2)
ras<-extend(ras, extent(-180,180,-90,90))
ras<-crop(ras, extent(-180,180,-90,90))
ras[is.na(ras)]<-0
ras[!is.na(ras)]<-0
#plot(ras)
names(ras)<-"value"
ras


bnamlist<-sort(bnamlist)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


darkcols1<-inferno(64, direction = 1)
colfunc1<-colorRampPalette(darkcols1, bias=2)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig3_1.png"
png(pngfile, width=16, height=18, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(5,3))

for(i in 1:15){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
    
  } else {
    dd_threatened<-ras
    dd_n<-ras
  }

  sst_longlat<-(dd_threatened)/(dd_n)
  
  #sst_longlat[sst_longlat>0.5]<-0.5
  #sst_longlat[is.na(sst_longlat)]<-0
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  plot(bb_poly_proj, col="grey35", bor="black", lwd=0.5)
  plot(admin0_poly_proj, col="grey30",bord="grey25", lwd=0.5, add=TRUE)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F
       ,zlim=c(0,1)
  )
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,0.2),
  #     axis.args=list(#at=seq(0,0.2,0.05),
  #labels=c("0%","5%",
  #         "10%","15%",">20%"), 
  #       cex.axis=0.5))
  #plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
  #plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(coast_lines_proj, col="grey50", lwd=0.01, add=TRUE)
  #plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}

dev.off()

##



pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig3_2.png"
png(pngfile, width=16, height=18*0.8, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(4,3))


for(i in 16:length(bnamlist)){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
    
  } else {
    dd_threatened<-ras
    dd_n<-ras
  }
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_n",bnamlist[i],sep=""))
  
  
  sst_longlat<-(dd_threatened)/(dd_n)
  
  #sst_longlat[sst_longlat>0.5]<-0.5
  #sst_longlat[is.na(sst_longlat)]<-0
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  plot(bb_poly_proj, col="grey35", bor="black", lwd=0.5)
  plot(admin0_poly_proj, col="grey30",bord="grey25", lwd=0.5, add=TRUE)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F
       ,zlim=c(0,1)
  )
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,0.2),
  #     axis.args=list(#at=seq(0,0.2,0.05),
  #labels=c("0%","5%",
  #         "10%","15%",">20%"), 
  #       cex.axis=0.5))
  #plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
  #plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(coast_lines_proj, col="grey50", lwd=0.01, add=TRUE)
  #plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}


dd_n<-mar_dd_n+terr_dd_n
dd_threatened<-mar_dd_threatened+terr_dd_threatened


sst_longlat<-(dd_threatened)/(dd_n)

#sst_longlat[sst_longlat>0.5]<-0.5
#sst_longlat[is.na(sst_longlat)]<-0

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="grey35", bor="black", lwd=0.5)
plot(admin0_poly_proj, col="grey30",bord="grey25", lwd=0.5, add=TRUE)
#plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F
     ,zlim=c(0,1)
)
#plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
#     horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,0.2),
#     axis.args=list(#at=seq(0,0.2,0.05),
#labels=c("0%","5%",
#         "10%","15%",">20%"), 
#       cex.axis=0.5))
#plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(coast_lines_proj, col="grey50", lwd=0.01, add=TRUE)
#plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

#mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
mtext("All Species", side=1, adj = 0, padj = 0, cex=0.7)


par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.5, zlim=c(0,1),
     horizontal=T, smallplot=c(0.2,0.8, 0.022,0.03), par(mar = par("mar")),outer=T,xpd=T,
     axis.args=list(at=seq(0,1,0.2),
                    labels=c("0%","20%","40%","60%","80%","100%"),
                    padj=-2.7,
                    tck=0,
                    cex.axis=0.7))



#par(opar)
dev.off()



par(mfrow=c(1,1))



##########################################################################


dd_threatened<-mar_dd_threatened+terr_dd_threatened

#extended data figure 4:
#################################
sst_longlat<-dd_threatened
sst_longlat[sst_longlat<1]<-NA

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=2)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig4_trp.png"
png(pngfile, width=16, height=11, units = "cm", res=900, bg = "transparent")

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))

plot(bb_poly_proj, col="gray20", bor="black", lwd=0.5)
#plot(grat30_lines_proj, col="gray25",add=T, lwd=0.4)
plot((sst_rob), col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot((sst_rob), legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(cex.axis=0.8, col.ticks="gray50",col.axis="gray50"))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.4)
#plot(admin0_poly_proj, col="gray15", bor="gray20", lwd=0.4, add=TRUE)
#plot(lrglakes_poly_proj, col=cividis(9, alpha = 0.25)[5], bor=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
#plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(admin0_lines_proj, col="gray20", lwd=0.4, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Number of Data Deficient species predicted to be threatened by extinction", col="gray50", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######
###################################



#extended data figure 5:
########################################################################

darkcols1<-c(rev(c(
  "#333333",
  "#0584B3",
  "#0FCBFA",
  "#73E1FC",
  "#B9EBF3"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]
colfunc1<-colorRampPalette(darkcols1, bias=1)



load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_dd_risk",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_dd_risk",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/mar_all_risk",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_n",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/terr_all_risk",sep=""))



dd_risk<-mar_dd_risk+terr_dd_risk
dd_n<-mar_dd_n+terr_dd_n
all_risk<-mar_all_risk+terr_all_risk
all_n<-mar_all_n+terr_all_n

sst_longlat<-(dd_risk/dd_n)-((all_risk-dd_risk)/(all_n-dd_n))
sst_longlat[is.na(sst_longlat)]<-0
#sst_longlat[sst_longlat==0]<-NA
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[sst_longlat<(-0.2)]<-(-0.2)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#bg_rob <- projectRaster(all_risk, crs=robin_crs)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig5.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))

plot(bb_poly_proj, col="gray20", bor="black", lwd=0.5)
#plot(grat30_lines_proj, col="gray25",add=T, lwd=0.4)
plot((sst_rob), col=colfunc1(512), zlim=c(-0.2,0.2),legend=F, add=TRUE, axes=F)
plot((sst_rob), legend.only=TRUE,zlim=c(-0.2,0.2), col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(-0.2,0.2,0.1),
                    labels=(c("< -20%","-10%","0%","10%","> 20%")), 
                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col="gray50",add=T, lwd=0.4)
#plot(admin0_poly_proj, col="gray40", bor="gray40", lwd=0.4, add=TRUE)
#plot(lrglakes_poly_proj, col=cividis(9, alpha = 0.25)[5], bor=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
#plot(coast_lines_proj, col=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
plot(admin0_lines_proj, col="gray40", lwd=0.4, add=TRUE)
#plot(coast_lines_proj, col="gray30", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Difference in average PE score between Data Deficient and data-sufficient species", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######


########################################################################



















#extended data figure 6:
##############################################
#increase decrease tax group

darkcols1<-c(rev(c(
  "#333333",
  "#0584B3",
  "#0FCBFA",
  "#73E1FC",
  "#B9EBF3"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]
colfunc1<-colorRampPalette(darkcols1, bias=1)


# plot and save
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig6_1.png"
png(pngfile, width=16, height=18, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(5,3))

for(i in 1:15){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
    
  } else {
    dd_risk<-all_n
    dd_risk[is.na(dd_risk)]<-0
    dd_risk[!is.na(dd_risk)]<-0
    dd_n<-dd_risk
  }
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_n",bnamlist[i],sep=""))
  
  
  sst_longlat<-(all_risk)/(all_n)-(all_risk-dd_risk)/(all_n-dd_n)
  sst_longlat<-sst_longlat/((all_risk-dd_risk)/(all_n-dd_n))
  #sst_longlat[is.na(sst_longlat)]<-0
  #sst_longlat[sst_longlat==0]<-NA
  sst_longlat[sst_longlat>0.2]<-0.2
  sst_longlat[sst_longlat<(-0.2)]<-(-0.2)
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="gray30", bor="black", lwd=0.5)
  plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
  plot(admin0_poly_proj, col="gray50", bor="gray50", lwd=0.5, add=TRUE)
  plot(lrglakes_poly_proj, col="gray30", bor="gray70", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.5, add=TRUE)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(-0.2,0.2))
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
  #     axis.args=list(at=seq(-0.2,0.2,0.1),
  #                    labels=c("< -20%","-10%",
  #                             "0%","10%","> 20%"), 
  #                    cex.axis=0.8))
  plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
  plot(admin0_lines_proj, col="gray50", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.3, add=TRUE)
  #plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
  #plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
  plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}

dev.off()


###
pngfile <- "C:/Users/janbor/Desktop/OneDrive - NTNU/ThreatLevelSubmission/figures/ext_data_fig6_2.png"
png(pngfile, width=16, height=18*0.8, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(4,3))


for(i in 16:length(bnamlist)){
  
  
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
    
  } else {
    dd_risk<-all_n
    dd_risk[is.na(dd_risk)]<-0
    dd_risk[!is.na(dd_risk)]<-0
    dd_n<-dd_risk
  }

  
  sst_longlat<-(all_risk)/(all_n)-(all_risk-dd_risk)/(all_n-dd_n)
  sst_longlat<-sst_longlat/((all_risk-dd_risk)/(all_n-dd_n))
  #sst_longlat[is.na(sst_longlat)]<-0
  #sst_longlat[sst_longlat==0]<-NA
  sst_longlat[sst_longlat>0.2]<-0.2
  sst_longlat[sst_longlat<(-0.2)]<-(-0.2)
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="gray30", bor="black", lwd=0.5)
  plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
  plot(admin0_poly_proj, col="gray50", bor="gray50", lwd=0.5, add=TRUE)
  plot(lrglakes_poly_proj, col="gray30", bor="gray70", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.5, add=TRUE)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(-0.2,0.2))
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
  #     axis.args=list(at=seq(-0.2,0.2,0.1),
  #                    labels=c("< -20%","-10%",
  #                             "0%","10%","> 20%"), 
  #                    cex.axis=0.8))
  plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
  plot(admin0_lines_proj, col="gray50", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.3, add=TRUE)
  #plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
  #plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
  plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}



dd_n<-mar_dd_n+terr_dd_n
dd_risk<-mar_dd_risk+terr_dd_risk
all_n<-mar_all_n+terr_all_n
all_risk<-mar_all_risk+terr_all_risk


sst_longlat<-(all_risk)/(all_n)-(all_risk-dd_risk)/(all_n-dd_n)
sst_longlat<-sst_longlat/((all_risk-dd_risk)/(all_n-dd_n))
#sst_longlat[is.na(sst_longlat)]<-0
#sst_longlat[sst_longlat==0]<-NA
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[sst_longlat<(-0.2)]<-(-0.2)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)



plot(bb_poly_proj, col="gray30", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
plot(admin0_poly_proj, col="gray50", bor="gray50", lwd=0.5, add=TRUE)
plot(lrglakes_poly_proj, col="gray30", bor="gray70", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray70", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(-0.2,0.2))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
plot(admin0_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray70", lwd=0.3, add=TRUE)
#plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
#plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("All Species", side=1, adj = 0, padj = 0, cex=0.7)


par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.5, zlim=c(-0.2,0.2),
     horizontal=T, smallplot=c(0.2,0.8, 0.022,0.03), par(mar = par("mar")),outer=T,xpd=T,
     axis.args=list(at=seq(-0.2,0.2,0.1),
                    labels=c("< -20%","-10%","0%","+10%","> +20%"),
                    padj=-2.7,
                    tck=0,
                    cex.axis=0.7))



#par(opar)
dev.off()
par(mfrow=c(1,1))

############################################################################




#####################################################################################


































all_risk<-mar_all_risk+terr_all_risk

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_contr_taxonomic1.png"
png(pngfile, width=16, height=18, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(5,3))

bnamlist<-sort(bnamlist)
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/mar_all_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/terr_all_risk_global2",sep=""))
all_risk<-mar_all_risk+terr_all_risk
all_risk<-terra::aggregate(all_risk, 2)

for(i in 1:15){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))
  } else {
    dd_risk<-ras
  }
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_n",bnamlist[i],sep=""))
  
  
  sst_longlat<-(dd_risk)/(all_risk)
  
  sst_longlat[sst_longlat>0.1]<-0.1
  sst_longlat[is.na(sst_longlat)]<-0
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F,zlim=c(0,0.1))
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,0.2),
  #     axis.args=list(#at=seq(0,0.2,0.05),
  #labels=c("0%","5%",
  #         "10%","15%",">20%"), 
  #       cex.axis=0.5))
  #plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
  #plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="grey60", lwd=0.01, add=TRUE)
  #plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}

dev.off()

pngfile <- "C:/Users/janbor/Desktop/output_contr_taxonomic2.png"
png(pngfile, width=16, height=18*0.8, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(4,3))


for(i in 16:length(bnamlist)){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_risk",bnamlist[i],sep=""))
  } else {
    dd_risk<-ras
  }
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_n",bnamlist[i],sep=""))
  
  
  sst_longlat<-(dd_risk)/(all_risk)
  
  sst_longlat[sst_longlat>0.1]<-0.1
  sst_longlat[is.na(sst_longlat)]<-0
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F,zlim=c(0,0.1))
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,0.2),
  #     axis.args=list(#at=seq(0,0.2,0.05),
  #labels=c("0%","5%",
  #         "10%","15%",">20%"), 
  #       cex.axis=0.5))
  #plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
  #plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="grey60", lwd=0.01, add=TRUE)
  #plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}




all_risk<-mar_all_risk+terr_all_risk
dd_risk<-mar_dd_risk+terr_dd_risk



sst_longlat<-(dd_risk)/(all_risk)

sst_longlat[sst_longlat>0.1]<-0.1
sst_longlat[is.na(sst_longlat)]<-0

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
#plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F,zlim=c(0,0.1))
#plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
#     horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,0.2),
#     axis.args=list(#at=seq(0,0.2,0.05),
#labels=c("0%","5%",
#         "10%","15%",">20%"), 
#       cex.axis=0.5))
#plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="grey60", lwd=0.01, add=TRUE)
#plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

#mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
#mtext("All species", side=3, adj = 0.1, padj = 2, cex=0.7)
mtext("All Species", side=1, adj = 0, padj = 0, cex=0.7)


par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.5, zlim=c(0,0.1),
     horizontal=T, smallplot=c(0.2,0.8, 0.022,0.03), par(mar = par("mar")),outer=T,xpd=T,
     axis.args=list(at=seq(0,0.1,0.02),
                    labels=c("0%","2%","4%","6%","8%","> 10%"),
                    padj=-2.7,
                    tck=0,
                    cex.axis=0.7))



#par(opar)
dev.off()



par(mfrow=c(1,1))

##



####################################################












#####


darkcols1<-c(rev(c(
  "#333333",
  "#0584B3",
  "#0FCBFA",
  "#73E1FC",
  "#B9EBF3"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]
colfunc1<-colorRampPalette(darkcols1, bias=1)


# plot and save
pngfile <- "C:/Users/janbor/Desktop/plots/output_incrdecr_threat_taxonomic1.png"
png(pngfile, width=16, height=18, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(5,3))

for(i in 1:15){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
    
  } else {
    dd_risk<-all_n
    dd_risk[is.na(dd_risk)]<-0
    dd_risk[!is.na(dd_risk)]<-0
  }
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
    
  } else {
    dd_n<-all_n
    dd_n[is.na(dd_n)]<-0
    dd_n[!is.na(dd_n)]<-0
  }
  
  
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_n",bnamlist[i],sep=""))
  
  
  sst_longlat<-(all_threatened)/(all_n)-(all_threatened-dd_threatened)/(all_n-dd_n)
  #sst_longlat[is.na(sst_longlat)]<-0
  #sst_longlat[sst_longlat==0]<-NA
  sst_longlat[sst_longlat>0.1]<-0.1
  sst_longlat[sst_longlat<(-0.1)]<-(-0.1)
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="gray30", bor="black", lwd=0.5)
  plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
  plot(admin0_poly_proj, col="gray50", bor="gray50", lwd=0.5, add=TRUE)
  plot(lrglakes_poly_proj, col="gray30", bor="gray70", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.5, add=TRUE)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(-0.1,0.1))
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
  #     axis.args=list(at=seq(-0.2,0.2,0.1),
  #                    labels=c("< -20%","-10%",
  #                             "0%","10%","> 20%"), 
  #                    cex.axis=0.8))
  plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
  plot(admin0_lines_proj, col=cividis(3, alpha=0.5)[2], lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.3, add=TRUE)
  #plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
  #plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
  plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}

dev.off()

#


























####


darkcols1<-viridis(64, direction = 1)
colfunc1<-colorRampPalette(darkcols1, bias=1)


# plot and save
pngfile <- "C:/Users/janbor/Desktop/plots/output_dd_fraction_taxonomic1.png"
png(pngfile, width=16, height=18, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(5,3))

for(i in 1:15){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
    
  } else {
    dd_n<-all_n
    dd_n[is.na(dd_n)]<-0
    dd_n[!is.na(dd_n)]<-0
  }
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_n",bnamlist[i],sep=""))
  
  
  sst_longlat<-(dd_n)/(all_n)
  #sst_longlat[is.na(sst_longlat)]<-0
  #sst_longlat[sst_longlat==0]<-NA
  sst_longlat[sst_longlat>0.2]<-0.2
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="gray30", bor="black", lwd=0.5)
  plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
  plot(admin0_poly_proj, col="gray50", bor="gray50", lwd=0.5, add=TRUE)
  plot(lrglakes_poly_proj, col="gray30", bor="gray70", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.5, add=TRUE)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(0,0.2))
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
  #     axis.args=list(at=seq(-0.2,0.2,0.1),
  #                    labels=c("< -20%","-10%",
  #                             "0%","10%","> 20%"), 
  #                    cex.axis=0.8))
  plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
  plot(admin0_lines_proj, col=cividis(3, alpha=0.5)[2], lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.3, add=TRUE)
  #plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
  #plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
  plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}

dev.off()



########


pngfile <- "C:/Users/janbor/Desktop/plots/output_dd_fraction_taxonomic2.png"
png(pngfile, width=16, height=18*0.8, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(4,3))


for(i in 16:length(bnamlist)){
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_n",bnamlist[i],sep=""))
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/all_n",bnamlist[i],sep=""))
  
  
  if(file.exists(paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))){
    load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
    
  } else {
    dd_n<-all_n
    dd_n[is.na(dd_n)]<-0
    dd_n[!is.na(dd_n)]<-0
  }
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_n",bnamlist[i],sep=""))
  
  
  sst_longlat<-(dd_n)/(all_n)
  #sst_longlat[is.na(sst_longlat)]<-0
  #sst_longlat[sst_longlat==0]<-NA
  sst_longlat[sst_longlat>0.2]<-0.2
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="gray30", bor="black", lwd=0.5)
  plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
  plot(admin0_poly_proj, col="gray50", bor="gray50", lwd=0.5, add=TRUE)
  plot(lrglakes_poly_proj, col="gray30", bor="gray70", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.5, add=TRUE)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(0,0.2))
  #plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
  #     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
  #     axis.args=list(at=seq(-0.2,0.2,0.1),
  #                    labels=c("< -20%","-10%",
  #                             "0%","10%","> 20%"), 
  #                    cex.axis=0.8))
  plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
  plot(admin0_lines_proj, col=cividis(3, alpha=0.5)[2], lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray70", lwd=0.3, add=TRUE)
  #plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
  #plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
  plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  mtext(((paste((firstup(tolower(bnamlist[i])))))), font=3, side=1, adj = 0.1, padj = 0, cex=0.7)
  
}


load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/mar_dd_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/mar_all_n_global2",sep=""))

load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/terr_dd_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Version2/predictions/GLOBAL/terr_all_n_global2",sep=""))

dd_n<-mar_dd_n+terr_dd_n
all_n<-mar_all_n+terr_all_n

sst_longlat<-(dd_n)/(all_n)
#sst_longlat[is.na(sst_longlat)]<-0
#sst_longlat[sst_longlat==0]<-NA
sst_longlat[sst_longlat>0.2]<-0.2
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)



plot(bb_poly_proj, col="gray30", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray40",add=T, lwd=0.5)
plot(admin0_poly_proj, col="gray50", bor="gray50", lwd=0.5, add=TRUE)
plot(lrglakes_poly_proj, col="gray30", bor="gray70", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray70", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(0,0.2))
#plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75, zlim=c(-0.2,0.2),
#     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
#     axis.args=list(at=seq(-0.2,0.2,0.1),
#                    labels=c("< -20%","-10%",
#                             "0%","10%","> 20%"), 
#                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col="gray40",add=T, lwd=0.5)
plot(admin0_lines_proj, col=cividis(3, alpha=0.5)[2], lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray70", lwd=0.3, add=TRUE)
#plot(grat30_lines_proj, col="gray50",add=T, lwd=0.1)
#plot(coast_lines_proj, col=magma(10, alpha = 0.5)[10], lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("All Species", side=1, adj = 0, padj = 0, cex=0.7)


par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.5, zlim=c(0,0.2),
     horizontal=T, smallplot=c(0.2,0.8, 0.022,0.03), par(mar = par("mar")),outer=T,xpd=T,
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%","10%","15%","> 20%"),
                    padj=-2.7,
                    tck=0,
                    cex.axis=0.7))



#par(opar)
dev.off()



par(mfrow=c(1,1))










#############
































########################################################
robin_crs <- CRS("+proj=robin +lon_0=0w")
bg_rob <- projectRaster(ras, crs=robin_crs)


darkcols1<-inferno(20)[1:20]
colfunc1<-colorRampPalette(darkcols1, bias=1)


#opar<-par()

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_avg_ext_risk_groups.png"
png(pngfile, width=16, height=10, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(2,3))

for(i in c(1:5,7)){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_risk_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_n_global2",bnam,sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_n_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_threatened_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_threatened_global2",bnam,sep=""))
  
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_risk_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_n_global2",bnam,sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_n_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_threatened_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_threatened_global2",bnam,sep=""))
  
  
  
  sst_longlat<-(terr_dd_risk+mar_dd_risk)/(terr_dd_n+mar_dd_n)
  
  #sst_longlat[sst_longlat>0.2]<-0.2
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F,zlim=c(0,1))
  plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
       horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,1),
       axis.args=list(#at=seq(0,0.2,0.05),
         #labels=c("0%","5%",
         #         "10%","15%",">20%"), 
         cex.axis=0.5))
  #plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
  #plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col=magma(10, alpha=0.3)[10], lwd=0.01, add=TRUE)
  #plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(paste(bnamlist[i]), side=3, adj = 0.1, padj = 2, cex=0.7)
  
}

par(mfrow=c(1,1))

#par(opar)
dev.off()
######



# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_contr_ext_risk_groups.png"
png(pngfile, width=16, height=24, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(4,2))

for(i in c(1:7)){
  
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_risk_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_n_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_n_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_threatened_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_threatened_global2",bnam,sep=""))
  
  
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_risk_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_n_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_n_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_threatened_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_threatened_global2",bnam,sep=""))
  
  
  
  sst_longlat<-(terr_dd_risk+mar_dd_risk)/(terr_all_risk+mar_all_risk)
  
  #sst_longlat[sst_longlat>0.2]<-0.2
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F,zlim=c(0,1))
  plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
       horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(0,1),
       axis.args=list(#at=seq(0,0.2,0.05),
         #labels=c("0%","5%",
         #         "10%","15%",">20%"), 
         cex.axis=0.5))
  #plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
  #plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col=magma(10, alpha=0.3)[10], lwd=0.01, add=TRUE)
  #plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(paste(bnamlist[i]), side=3, adj = 0.1, padj = 2, cex=0.7)
  
}

par(mfrow=c(1,1))

#par(opar)
dev.off()
######




# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_dd_threatened_groups.png"
png(pngfile, width=16, height=25, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(3,2))

for(i in c(1:5,7)){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_n_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_n_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_threatened_global2",bnam,sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_threatened_global2",bnamlist[i],sep=""))
  
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_n_global2",bnam,sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_n_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_threatened_global2",bnam,sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_threatened_global2",bnamlist[i],sep=""))
  
  
  
  sst_longlat<-(terr_dd_threatened+mar_dd_threatened)
  
  sst_longlat[sst_longlat<1]<-NA
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="gray70", bor="black", lwd=0.5)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(grat30_lines_proj, col="gray60", lwd=1.0, add=TRUE)
  plot(admin0_poly_proj, col="grey80",bor="grey80", lwd=0.5, add=TRUE)
  plot(lrglakes_poly_proj, col="gray70", bor="gray60", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col="gray60", lwd=0.5, add=TRUE)
  
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
  plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
       horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),
       axis.args=list(#at=seq(0,0.2,0.05),
         #labels=c("0%","5%",
         #         "10%","15%",">20%"), 
         cex.axis=0.5))
  
  plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(paste(bnamlist[i]), side=3, adj = 0.1, padj = 2, cex=0.7)
  
}

par(mfrow=c(1,1))

#par(opar)
dev.off()
######







darkcols1<-brewer.pal(11,"Spectral")
#darkcols1<-rev(darkcols1)
#darkcols1<-darkcols1[c(1:4,6,8:11)]
colfunc1<-colorRampPalette(darkcols1, bias=1)

all_n<-ras
threatened_n<-ras
threatened_p<-ras
# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_diff_threatened_n_groups.png"
png(pngfile, width=16, height=10, units = "cm", res=900)

par(mar=c(3,0,0,0))
par(mfrow=c(2,3))



for(i in c(1:5,7)){
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_risk_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_n_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_n_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_threatened_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_threatened_n_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_threatened_global2",bnamlist[i],sep=""))
  
  
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_risk_global2",bnamlist[i],sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_risk_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_n_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_n_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_threatened_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_threatened_n_global2",bnamlist[i],sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_threatened_global2",bnamlist[i],sep=""))
  
  
  
  tsst_longlat<-(mar_all_threatened+terr_all_threatened)-(mar_dd_threatened+terr_dd_threatened)
  tsst_longlat[is.na(tsst_longlat)]<-0
  
  tnsst_longlat<-(mar_all_threatened_n+terr_all_threatened_n)
  tnsst_longlat[is.na(tnsst_longlat)]<-0
  
  nsst_longlat<-(mar_all_n+terr_all_n)-(mar_dd_n+terr_dd_n)
  nsst_longlat[is.na(nsst_longlat)]<-0
  
  
  all_n<-all_n+nsst_longlat
  threatened_n<-threatened_n+tnsst_longlat
  threatened_p<-threatened_p+tsst_longlat
  
  sst_longlat<-(threatened_p/all_n)-(threatened_n/all_n)
  
  sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
  
  
  
  plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
  #plot(bg_rob, col="grey40", legend=F, add=TRUE, axes=F)
  plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F,zlim=c(-max(c(maxValue(sst_rob),-minValue(sst_rob))),max(c(maxValue(sst_rob),-minValue(sst_rob)))))
  plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
       horizontal=TRUE, smallplot=c(0.1,0.9, 0.2,0.24), par(mar = par("mar")),zlim=c(-max(c(maxValue(sst_rob),-minValue(sst_rob))),max(c(maxValue(sst_rob),-minValue(sst_rob)))),
       axis.args=list(#at=seq(0,0.2,0.05),
         #labels=c("0%","5%",
         #         "10%","15%",">20%"), 
         cex.axis=0.5))
  #plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
  #plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  #plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
  plot(coast_lines_proj, col=magma(10, alpha=0.3)[10], lwd=0.01, add=TRUE)
  #plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
  
  #mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -5, cex=0.5)
  mtext(paste(bnamlist[i]), side=3, adj = 0.1, padj = 2, cex=0.7)
  
}

par(mfrow=c(1,1))

#par(opar)
dev.off()
######




load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_threatened_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_threatened_global2",sep=""))

mar_all_risk[mar_all_risk<75]<-NA
terr_all_risk[terr_all_risk<10]<-NA


all_mar_dd_risk<-mar_dd_risk
all_mar_dd_threatened<-mar_dd_threatened
all_terr_dd_risk<-terr_dd_risk
all_terr_dd_threatened<-terr_dd_threatened

plot(all_mar_dd_risk)
plot(all_mar_dd_threatened)
plot(all_terr_dd_risk)
plot(all_terr_dd_threatened)

all_mar_dd_risk[is.na(mar_all_risk)]<-NA
all_terr_dd_risk[is.na(terr_all_risk)]<-NA
plot(all_mar_dd_risk)
plot(all_mar_dd_threatened)
plot(all_terr_dd_risk)
plot(all_terr_dd_threatened)


udirs<-list.dirs("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Reference/Version2020-3/", recursive = F, full.names = T)
bnamlist<-c()
par(mfrow=c(2,2))

for(x in 1:length(udirs)){
  dirs<-list.dirs(udirs[x])
  
  if(length(dirs)>1){
    dirs<-dirs[2:length(dirs)]
  }
  
  
  bnam<-basename(udirs[x])
  bnamlist<-c(bnamlist, bnam)
  
  for(d in 1:length(dirs)){
    
    
    shp_files<-list.files(paste(dirs[d],sep=""), pattern = ".shp$", recursive = T, full.names = T)
    
    for(a in 1:length(shp_files)){
      
      
      terr_dd_risk<-ras
      terr_dd_threatened<-ras
      mar_dd_risk<-ras
      mar_dd_threatened<-ras
      
      bnam<-str_remove(str_split(shp_files[a],pattern = "/")[[1]][length(str_split(shp_files[a],pattern = "/")[[1]])]
                       ,".shp")
      
      
      
      
      tryCatch({
        load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_risk_global2",bnam,sep=""))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      tryCatch({
        load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_threatened_global2",bnam,sep=""))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      tryCatch({
        load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_risk_global2",bnam,sep=""))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      tryCatch({
        load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_threatened_global2",bnam,sep=""))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
      
      
      
      if(maxValue(terr_dd_risk)==0){
        terr_dd_risk<-ras
      }
      if(maxValue(terr_dd_threatened)==0){
        terr_dd_threatened<-ras
      }
      
      if(maxValue(mar_dd_risk)==0){
        mar_dd_risk<-ras
      }
      if(maxValue(mar_dd_threatened)==0){
        mar_dd_threatened<-ras
      }
      
      
      
      
      pp<-terr_dd_risk/all_terr_dd_risk
      pp[is.na(pp)]<-0
      pp[pp>0.5]<-0.5
      
      
      plot(pp, 
           main=paste("risk non-Marine",str_split(shp_files[a],pattern = "/")[[1]][length(str_split(shp_files[a],pattern = "/")[[1]])],sep=" "), zlim=c(0,0.5))
      plot(coast_lines, col="grey90", lwd=0.5,add=T)
      
      pp<-terr_dd_threatened/all_terr_dd_threatened
      pp[is.na(pp)]<-0
      pp[pp>0.5]<-0.5
      
      plot(pp, 
           main=paste("threat non-Marine",str_split(shp_files[a],pattern = "/")[[1]][length(str_split(shp_files[a],pattern = "/")[[1]])],sep=" "), zlim=c(0,0.5))
      plot(coast_lines, col="grey90", lwd=0.5,add=T)
      
      
      pp<-mar_dd_risk/all_mar_dd_risk
      pp[is.na(pp)]<-0
      pp[pp>0.5]<-0.5
      
      plot(pp, 
           main=paste("risk Marine",str_split(shp_files[a],pattern = "/")[[1]][length(str_split(shp_files[a],pattern = "/")[[1]])],sep=" "), zlim=c(0,0.5))
      plot(coast_lines, col="grey90", lwd=0.5,add=T)
      
      pp<-mar_dd_threatened/all_mar_dd_threatened
      pp[is.na(pp)]<-0
      pp[pp>0.5]<-0.5
      
      plot(pp,
           main=paste("threat Marine",str_split(shp_files[a],pattern = "/")[[1]][length(str_split(shp_files[a],pattern = "/")[[1]])],sep=" "),zlim=c(0,0.5))
      plot(coast_lines, col="grey90", lwd=0.5,add=T)
      
    }
    
    
    
    
  }
  
  
  
  
  
}







#plots
#######
#plots

load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_global2",sep=""))
#load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/threatened_n_global",sep=""))


load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_all_threatened_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/mar_dd_threatened_global2",sep=""))


load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_risk_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_n_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_all_threatened_global2",sep=""))
load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/terr_dd_threatened_global2",sep=""))




all_risk<-mar_all_risk+terr_all_risk
dd_risk<-mar_dd_risk+terr_dd_risk
all_n<-mar_all_n+terr_all_n
dd_n<-mar_dd_n+terr_dd_n
all_threatened<-mar_all_threatened+terr_all_threatened
dd_threatened<-mar_dd_threatened+terr_dd_threatened

mar_contr<-mar_dd_risk/mar_all_risk
terr_contr<-terr_dd_risk/terr_all_risk

mar_contr<-(mar_dd_threatened)/(mar_dd_n)
terr_contr<-terr_dd_threatened/terr_dd_n


mar_contr<-(mar_all_risk)/(mar_all_n)-(mar_all_risk-mar_dd_risk)/(mar_all_n-mar_dd_n)
mar_contr<-mar_contr/((mar_all_risk-mar_dd_risk)/(mar_all_n-mar_dd_n))
mar_contr[is.na(mar_contr)]<-0
mar_contr[mar_contr>1]<-1
mar_contr[mar_contr<0]<-0
terr_contr<-(terr_all_risk)/(terr_all_n)-(terr_all_risk-terr_dd_risk)/(terr_all_n-terr_dd_n)
terr_contr<-terr_contr/((terr_all_risk-terr_dd_risk)/(terr_all_n-terr_dd_n))
terr_contr[is.na(terr_contr)]<-0
terr_contr[terr_contr>1]<-1
terr_contr[terr_contr<0]<-0

contr<-(all_risk)/(all_n)-(all_risk-dd_risk)/(all_n-dd_n)
contr<-contr/((all_risk-dd_risk)/(all_n-dd_n))
contr[is.na(contr)]<-0
contr[contr>0.2]<-0.2
contr[contr<(-0.2)]<-(-0.2)

darkcols1<-c(rev(c(
  "#000000",
  "#041f32",
  "#0478a1",
  "#04aed9",
  "#82d7ec",
  "#ffffff"
)),(inferno(6)
)
)



darkcols1<-darkcols1[-c(5,7,8)]
colfunc1<-colorRampPalette(darkcols1, bias=1)
plot(contr, col=colfunc1(512), zlim=c(-0.2,0.2))


contr<-(all_risk)/(all_n)-(all_risk-dd_risk)/(all_n-dd_n)
contr<-contr/((all_risk-dd_risk)/(all_n-dd_n))
contr[is.na(contr)]<-0
contr<-contr*-1
contr[contr>0.5]<-0.5
contr[contr<0]<-0

darkcols1<-c(
  "#000000",
  "#041f32",
  "#003853",
  "#0478a1",
  "#04aed9",
  "#ffffff"
)
colfunc1<-colorRampPalette(darkcols1, bias=1.5)
plot(contr, col=colfunc1(512))



plot(mar_contr, col=colfunc1(64))
plot(terr_contr, col=colfunc1(64))
plot(contr, col=colfunc1(64))


darkcols1<-brewer.pal(11,"RdYlBu")
darkcols1<-rev(darkcols1)
darkcols1<-darkcols1[-c(4,5,7,8)]
colfunc1<-colorRampPalette(darkcols1, bias=1)
plot(contr, col=colfunc1(512), zlim=c(-0.05,0.05))


plot(mar_contr, col=turbo(64))
plot(terr_contr, col=turbo(64))


#

sst_longlat<-dd_risk/all_risk
#sst_longlat/dd_n
sst_longlat[sst_longlat>0.2]<-0.2

plot(sst_longlat, col=inferno(512))

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

bg_rob <- projectRaster(all_risk, crs=robin_crs)


darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=0.6)



# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_contribution.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col=inferno(10)[1], bor="black", lwd=0.5)
plot(bg_rob, col=colfunc1(10)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%",
                             "10%","15%","> 20%"), 
                    cex.axis=0.8))
#plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray30", lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######



darkcols1<-c(rev(c(
  "#333333",
  "#0584B3",
  "#0FCBFA",
  "#73E1FC",
  "#B9EBF3"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]
colfunc1<-colorRampPalette(darkcols1, bias=1)


sst_longlat<-(all_risk)/(all_n)-(all_risk-dd_risk)/(all_n-dd_n)
sst_longlat<-sst_longlat/((all_risk-dd_risk)/(all_n-dd_n))
sst_longlat[is.na(sst_longlat)]<-0
#sst_longlat[sst_longlat==0]<-NA
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[sst_longlat<(-0.2)]<-(-0.2)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_incrdecr.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col=inferno(10)[1], bor="black", lwd=0.5)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(-0.2,0.2,0.1),
                    labels=c("< -20%","- 10%",
                             "0%","10%","> 20%"), 
                    cex.axis=0.8))
#plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray30", lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Change in average predicted extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######





sst_longlat<-dd_risk
sst_longlat[sst_longlat<0.5]<-NA

sst_longlat<-dd_n/all_n
sst_longlat[sst_longlat>0.2]<-0.2
plot(sst_longlat, col=viridis(512))

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#bg_rob <- projectRaster(all_risk, crs=robin_crs)


darkcols1<-viridis(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=0.6)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_species_map.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col=inferno(10)[1], bor="black", lwd=0.5)
plot(bg_rob, col=colfunc1(64)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%",
                             "10%","15%","> 20%"), 
                    cex.axis=0.8))
#plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray30", lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Fraction of data-deficient species", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######


#####




dd_dat_t<-subset(dd_dat, dd_dat$predict=="threatened")
#dd_dat_t<-dd_dat
#dd_dat_t<-subset(dd_dat_t, dd_dat_t$marine=="false")
dd_dat_t<-subset(dd_dat_t, dd_dat_t$category=="DD")
plot(density(dd_dat_t$threatened))
nrow(subset(dd_dat_t, dd_dat_t$threatened>0.9))
table(dd_dat_t$marine)
plot(mar_dd_threatened/(mar_dd_n))
plot(terr_dd_threatened/terr_dd_n)

###








darkcols1<-c(rev(c(
  "#333333",
  "#0584B3",
  "#0FCBFA",
  "#73E1FC",
  "#B9EBF3"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]
colfunc1<-colorRampPalette(darkcols1, bias=1)



sst_longlat<-(dd_risk/dd_n)-((all_risk-dd_risk)/(all_n-dd_n))
sst_longlat[is.na(sst_longlat)]<-0
#sst_longlat[sst_longlat==0]<-NA
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[sst_longlat<(-0.2)]<-(-0.2)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#bg_rob <- projectRaster(all_risk, crs=robin_crs)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_dd_ds.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))

plot(bb_poly_proj, col="gray20", bor="black", lwd=0.5)
#plot(grat30_lines_proj, col="gray25",add=T, lwd=0.4)
plot((sst_rob), col=colfunc1(512), zlim=c(-0.2,0.2),legend=F, add=TRUE, axes=F)
plot((sst_rob), legend.only=TRUE,zlim=c(-0.2,0.2), col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(-0.2,0.2,0.1),
                    labels=(c("< -20%","-10%","0%","10%","> 20%")), 
                    cex.axis=0.8))
plot(grat30_lines_proj_ocean, col=cividis(9, alpha = 0.2)[5],add=T, lwd=0.4)
plot(admin0_poly_proj, col=cividis(9, alpha = 0.15)[5], bor=cividis(9, alpha = 0.2)[5], lwd=0.4, add=TRUE)
#plot(lrglakes_poly_proj, col=cividis(9, alpha = 0.25)[5], bor=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
#plot(coast_lines_proj, col=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
#plot(admin0_lines_proj, col=cividis(3, alpha=0.5)[2], lwd=0.4, add=TRUE)
#plot(coast_lines_proj, col="gray30", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Difference in average PE score between data-deficient and data-sufficient species", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######








darkcols1<-c(rev(c(
  "#333333",
  "#0584B3",
  "#0FCBFA",
  "#73E1FC",
  "#B9EBF3"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]
darkcols1<-darkcols1[-c(1:5)]
darkcols1<-inferno(10)
colfunc1<-colorRampPalette(darkcols1, bias=2)


sst_longlat<-dd_threatened
sst_longlat[sst_longlat==0]<-NA
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#bg_rob <- projectRaster(all_risk, crs=robin_crs)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_dd_threatened.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))

plot(bb_poly_proj, col="gray20", bor="black", lwd=0.5)
#plot(grat30_lines_proj, col="gray25",add=T, lwd=0.4)
plot((sst_rob), col=colfunc1(512),legend=F, add=TRUE, axes=F)
plot((sst_rob), legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(cex.axis=0.8, at=c(1,10,20,30,40,50,round(maxValue(sst_rob))), 
                    labels=c(1,10,20,30,40,50,round(maxValue(sst_rob)))))
plot(grat30_lines_proj_ocean, col=cividis(9, alpha = 0.2)[5],add=T, lwd=0.4)
plot(admin0_poly_proj, col=cividis(9, alpha = 0.15)[5], bor=cividis(9, alpha = 0.2)[5], lwd=0.4, add=TRUE)
#plot(lrglakes_poly_proj, col=cividis(9, alpha = 0.25)[5], bor=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
#plot(coast_lines_proj, col=cividis(9, alpha = 0.3)[5], lwd=0.5, add=TRUE)
#plot(admin0_lines_proj, col=cividis(3, alpha=0.5)[2], lwd=0.4, add=TRUE)
#plot(coast_lines_proj, col="gray30", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
plot((sst_rob), col=colfunc1(512),legend=F, add=TRUE, axes=F)
mtext("Number of data-deficient species predicted to be threatened by extinction", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######





####










sst_longlat<-dd_risk/dd_n
sst_longlat[is.na(sst_longlat)]<-0
sst_longlat[sst_longlat>0.5]<-0.5

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#bg_rob <- projectRaster(all_risk, crs=robin_crs)

darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=1)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_dd_risk.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))

plot(bb_poly_proj, col="gray12", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray30",add=T, lwd=0.1)
plot(admin0_poly_proj, col="gray30", bor="gray30", lwd=0.01, add=TRUE)
plot(lrglakes_poly_proj, col="gray12", bor="gray50", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot((sst_rob), col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot((sst_rob), legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.5,0.1),
                    labels=(c("0%","10%","20%","30%","40%","> 50%")), 
                    cex.axis=0.8))
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Average predicted extinction risk of data-deficient species", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######












darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=1)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_dd_threatened_sep.png"
png(pngfile, width=16, height=6, units = "cm", res=900)


opar<-par()
par(mfrow=c(1,2))
par(mar=c(3,0,0,0))

sst_longlat<-mar_dd_threatened/mar_dd_n
sst_longlat[sst_longlat>0.5]<-0.5
#sst_longlat[is.na(sst_longlat)]<-0
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="gray12", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray30",add=T, lwd=0.1)
plot(admin0_poly_proj, col="gray30", bor="gray30", lwd=0.01, add=TRUE)
plot(lrglakes_poly_proj, col="gray12", bor="gray50", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.5,0.1),
                    labels=c("0%","10%",
                             "20%","30%","40%","> 50%"), 
                    cex.axis=0.8))
plot(grat30_lines_proj, col="gray30",add=T, lwd=0.1)
plot(coast_lines_proj, col="gray50", lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Fraction of marine species", side=1, adj = 0.1, padj = -1, cex=0.8)

sst_longlat<-terr_dd_threatened/terr_dd_n
sst_longlat[sst_longlat>0.5]<-0.5
#sst_longlat[is.na(sst_longlat)]<-0
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="gray12", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray30",add=T, lwd=0.1)
plot(admin0_poly_proj, col="gray30", bor="gray30", lwd=0.01, add=TRUE)
plot(lrglakes_poly_proj, col="gray12", bor="gray50", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.5,0.1),
                    labels=c("0%","10%",
                             "20%","30%","40%","> 50%"), 
                    cex.axis=0.8))
plot(coast_lines_proj, col="gray50", lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Fraction of non-marine species", side=1, adj = 0.1, padj = -1, cex=0.8)



par(mfrow=c(1,1))

par(opar)
dev.off()
######



# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_contr_sep.png"
png(pngfile, width=16, height=6, units = "cm", res=900)


darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=1)

opar<-par()
par(mfrow=c(1,2))
par(mar=c(3,0,0,0))

sst_longlat<-mar_dd_risk/mar_all_risk
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[is.na(sst_longlat)]<-0
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="gray12", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray30",add=T, lwd=0.1)
plot(admin0_poly_proj, col="gray30", bor="gray30", lwd=0.01, add=TRUE)
plot(lrglakes_poly_proj, col="gray12", bor="gray50", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%",
                             "10%","15%","> 20%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="gray30",add=T, lwd=0.1)
plot(coast_lines_proj, col="gray50", lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("marine species", side=1, adj = 0.1, padj = -1, cex=0.8)


sst_longlat<-terr_dd_risk/terr_all_risk
sst_longlat[sst_longlat>0.2]<-0.2
sst_longlat[is.na(sst_longlat)]<-0
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col="gray12", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="gray30",add=T, lwd=0.1)
plot(admin0_poly_proj, col="gray30", bor="gray30", lwd=0.01, add=TRUE)
plot(lrglakes_poly_proj, col="gray12", bor="gray50", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="gray50", lwd=0.5, add=TRUE)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.18,0.2), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%",
                             "10%","15%","> 20%"), 
                    cex.axis=0.8))
plot(coast_lines_proj, col="gray50", lwd=0.3, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("non-marine species", side=1, adj = 0.1, padj = -1, cex=0.8)



par(mfrow=c(1,1))

par(opar)
dev.off()
######














darkcols1<-c(rev(c(
  "#000000",
  "#041f32",
  "#0478a1",
  "#04aed9",
  "#82d7ec",
  "#ffffff"
)),(inferno(6)
)
)
darkcols1<-darkcols1[-c(5,7,8)]



darkcols1<-c(rev(c(
  "#000000",
  "#0584B3",
  "#0FCBFA",
  "#AFEEFD",
  "#DAF6FC"
)),(inferno(6)
)
)
darkcols1
darkcols1<-darkcols1[-c(6,7)]

darkcols1[6]<-"#A9374A"

colfunc1<-colorRampPalette(darkcols1, bias=1)










####

###

darkcols1<-brewer.pal(11,"Spectral")
darkcols1<-rev(darkcols1)
darkcols1<-darkcols1[c(1:4,6,8:11)]
darkcols1[5]<-"grey90"

darkcols1<-turbo(11)
darkcols1[6]<-"grey90"
darkcols1<-darkcols1[-c(5,7)]
#darkcols1<-rev(darkcols1)
colfunc1<-colorRampPalette(darkcols1, bias=1)


sst_longlat<-(dd_risk/dd_n)-((all_risk-dd_risk)/(all_n-dd_n))
sst_longlat<-(mar_dd_risk/mar_dd_n)-((mar_all_risk-mar_dd_risk)/(mar_all_n-mar_dd_n))
sst_longlat<-((mar_all_threatened/mar_all_n)-((mar_all_threatened-mar_dd_threatened)/(mar_all_n-mar_dd_n))
)#/(((mar_all_threatened-mar_dd_threatened)/(mar_all_n-mar_dd_n)))

sst_longlat<-((terr_all_threatened/terr_all_n)-((terr_all_threatened-terr_dd_threatened)/(terr_all_n-terr_dd_n))
)#/(((terr_all_threatened-terr_dd_threatened)/(terr_all_n-terr_dd_n)))

sst_longlat<-((mar_all_risk/mar_all_n)-((mar_all_risk-mar_dd_risk)/(mar_all_n-mar_dd_n))
)/(((mar_all_risk-mar_dd_risk)/(mar_all_n-mar_dd_n)))

sst_longlat<-((terr_all_risk/terr_all_n)-((terr_all_risk-terr_dd_risk)/(terr_all_n-terr_dd_n))
)/(((terr_all_risk-terr_dd_risk)/(terr_all_n-terr_dd_n)))

#sst_longlat<-(terr_dd_risk/terr_dd_n)-((terr_all_risk-terr_dd_risk)/(terr_all_n-terr_dd_n))


sst_longlat<-((all_risk/all_n)-((all_risk-dd_risk)/(all_n-dd_n)))#/(((all_risk-dd_risk)/(all_n-dd_n)))

sst_longlat[dd_n<1]<-NA



sst_longlat[sst_longlat>0.05]<-0.05
sst_longlat[sst_longlat<(-0.05)]<-(-0.05)
#sst_longlat[is.na(sst_longlat)]<-0

plot(sst_longlat, col=colfunc1(64), zlim=c(-0.05,0.05))



plot(sst_longlat, col="black",zlim=c(-0.001,0.001))


d<-density(sst_longlat)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

#bg_rob <- projectRaster(all_risk, crs=robin_crs)



# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_incrdecr.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col=colfunc1(3)[2], bor="black", lwd=0.5)
plot(bg_rob, col=colfunc1(3)[2], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F, zlim=c(-0.2,0.2))
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, zlim=c(-0.2,0.2),smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(-0.2,0.2,0.1),
                    labels=c("< -20%","-10%",
                             "0%","+10%","> +20%"), 
                    cex.axis=0.8))
#plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Difference in average predicted extinction risk between data-deficient and data-sufficient species", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######



darkcols1<-brewer.pal(11,"Spectral")
darkcols1<-rev(darkcols1)
darkcols1<-darkcols1[c(1:4,6,8:11)]
colfunc1<-colorRampPalette(darkcols1, bias=1)

sst_longlat<-(dd_risk/all_risk)-(dd_n/all_n)
minValue(sst_longlat)
maxValue(sst_longlat)
sst_longlat[sst_longlat>0.1]<-0.1
sst_longlat[sst_longlat<(-0.1)]<-(-0.1)

plot(sst_longlat, col=colfunc1(64))

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

#bg_rob <- projectRaster(all_risk, crs=robin_crs)



# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_expect_observed.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col=colfunc1(3)[2], bor="black", lwd=0.5)
plot(bg_rob, col=colfunc1(3)[2], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(512), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(512), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=c((-0.2),(-0.1),0,0.1,0.2),
                    labels=c("< -20%","< -10%",
                             "0%","> 10%","> 20%"), 
                    cex.axis=0.8))
#plot(admin0_lines_proj, col="grey", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Difference between expected and observed contribution of data-deficient species to extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()
######







sst_longlat<-(dd_risk/dd_n)
minValue(sst_longlat)
maxValue(sst_longlat)
sst_longlat[sst_longlat>0.5]<-0.5
plot(sst_longlat, col=inferno(64))


#








####
##




#


sorted_dd<-aggregate(preds_all_dd$threatened~preds_all_dd$class, FUN=median)
sorted_dd<-sorted_dd[order(sorted_dd$`preds_all_dd$threatened`), ]

preds_all_dd$class<-factor(preds_all_dd$class, levels = sorted_dd$`preds_all_dd$class`,labels = sorted_dd$`preds_all_dd$class`)

p<-ggplot(preds_all_dd, aes(x = threatened, y = class, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5, panel_scaling = F) +
  scale_fill_viridis(name = "threatened", option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("Taxonomic group") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "", legend.title = element_blank())
p
pngfile <- "C:/Users/janbor/Desktop/plots/output_taxonomic_dd.png"
png(pngfile, width=16*1.5, height=20*1.5, units = "cm", res=900)
par(mar=c(4,4,3,0.5))
par(mfrow=c(1,1))
p
dev.off()
par(mfrow=c(1,1))

#

























preds_all_mar<-subset(preds_all, preds_all$marine=="true")

p<-ggplot(preds_all_mar, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges(panel_scaling = T, scale=1) +
  scale_fill_viridis_d(2, alpha = 0.7, direction = 1, option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("Taxonomic group") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

preds_all_nonmar<-subset(preds_all, preds_all$marine!="true")

p<-ggplot(preds_all_nonmar, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges(panel_scaling = T, scale=1) +
  scale_fill_viridis_d(2, alpha = 0.7, direction = 1, option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("Taxonomic group") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p







table(preds_all$marine)
table(preds_all$phylum)
table(preds_all$compartement)


#
preds_all_mar<-subset(preds_all, tolower(preds_all$marine)=="true")
#
preds_all_terr<-subset(preds_all, tolower(preds_all$marine)!="true")

preds_all_mar_outd<-subset(preds_all_mar, preds_all_mar$needs_update=="Yes")
table(preds_all_mar_outd$class)

for(i in 1:length(unique(preds_all_mar$class))){
  print(paste(unique(preds_all_mar$class)[i]," need update: ", 
              length(which(preds_all_mar$class==unique(preds_all_mar$class)[i] & (preds_all_mar$needs_update=="Yes")))/
                length(which(preds_all_mar$class==unique(preds_all_mar$class)[i]))
              ," remaining: ", length(which(preds_all_mar$class==unique(preds_all_mar$class)[i] & (preds_all_mar$needs_update=="No"))),sep=""))
}

length(which(preds_all_mar$class==unique(preds_all_mar$class)[15] & preds_all_mar$needs_update=="No"))

# those species that are known in marine species may be those that are most abundant
# greater range, etc. (endemic marine hard to spot)
table(preds_all_mar$category)

#without species that "need update"
table(preds_all_mar$category_group)

table(preds_all_mar$class)
table(preds_all_mar$phylum)
table(preds_all$systems)
table(preds_all_mar$needs_update)

p<-ggplot(preds_all_mar, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges(panel_scaling = TRUE, na.rm = T,
                      rel_min_height=0.0) +
  scale_fill_viridis_d(2, alpha = 0.8, direction = -1, option = "E") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

p<-ggplot(preds_all_terr, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges(panel_scaling = TRUE, na.rm = T,
                      rel_min_height=0.0) +
  scale_fill_viridis_d(2, alpha = 0.8, direction = -1, option = "E") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p


preds_all_act<-subset(preds_all_mar, preds_all_mar$class=="ACTINOPTERYGII")
table(preds_all_act$category)
#without species that "need update":
table(preds_all_act$category_group)
table(preds_all_act$class)
table(preds_all_act$needs_update)
sort(table(preds_all_act$order_))

preds_all_act_upd<-preds_all_act[which(preds_all_act$needs_update=="Yes"),]
sort(table(preds_all_act_upd$order_), decreasing=F)
# for those groups that in majority "need update",
# use species for training
preds_all_act<-subset(preds_all_mar, preds_all_mar$class=="ANTHOZOA")
table(preds_all_act$category)
#without species that "need update":
table(preds_all_act$category_group)
table(preds_all_act$class)
sort(table(preds_all_act$order_))


p<-ggplot(preds_all_act, aes(x = threatened, y = order_, fill = category_group)) +
  geom_density_ridges(panel_scaling = TRUE, na.rm = T,
                      rel_min_height=0.0) +
  scale_fill_viridis_d(2, alpha = 0.8, direction = -1, option = "E") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

#










preds_all_act<-subset(preds_all_mar, preds_all_mar$class=="CHONDRICHTHYES")
table(preds_all_act$category)
#without species that "need update":
table(preds_all_act$category_group)
table(preds_all_act$class)
sort(table(preds_all_act$order_))


preds_all_act<-subset(preds_all_mar, preds_all_mar$class=="MALACOSTRACA")
table(preds_all_act$category)
#without species that "need update":
table(preds_all_act$category_group)
table(preds_all_act$class)
sort(table(preds_all_act$order_))


# model with species that need update for groups where those species are majority
# indicate groups in plots with '*'






p<-ggplot(preds_all_act, aes(x = threatened, y = order_, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.8, direction = -1, option = "E") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

head(sort(table(preds_all_act$family), decreasing = T))

#




preds_all_cni<-subset(preds_all, preds_all$phylum=="CNIDARIA")
table(preds_all_cni$category)

d<-density(preds_all_cni$yrcompiled)
plot(d)
unique(preds_all_cni$yrcompiled)
nrow(preds_all_cni)
d<-density(preds_all_cni$threatened)
plot(d)

p<-ggplot(preds_all_cni, aes(x = threatened, y = family, fill = category)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.9, direction = -1, option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p
table(preds_all_cni$family)




#










rl_needs_update<-read.csv("X:/indecol/USERS/JanB/Data/iucn_gbif/ML/rl_needs_update/assessments.csv")
preds_all$needs_update<-"No"
for(i in 1:nrow(preds_all)){
  if(preds_all$binomial[i] %in% rl_needs_update$scientificName){
    preds_all$needs_update[i]<-"Yes"  
  }
}
#preds_all<-subset(preds_all, preds_all$needs_update=="No")


dd_dat<-cbind(preds_all)
dd_dat<-subset(dd_dat, dd_dat$category!="EX")
dd_dat<-subset(dd_dat, dd_dat$category!="EW")



dd_dat_groups<-dd_dat
dd_dat_groups<-subset(dd_dat_groups, dd_dat_groups$category!="EW")
dd_dat_groups<-subset(dd_dat_groups, dd_dat_groups$category!="EX")

dd_dat_groups$groups<-NA
dd_dat_groups2<-data.frame()
nam<-list.dirs("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Reference/Version2020-3/", recursive = F, full.names = F)
dirs<-list.dirs("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/Reference/Version2020-3/", recursive = F, full.names = T)

for(d in 1:length(dirs)){
  shp_files<-list.files(paste(dirs[d],sep=""), pattern = ".shp$", recursive = T, full.names = T)
  print(d)
  for(a in 1:length(shp_files)){
    
    shapes <- st_read(shp_files[a])
    for(i in 1:nrow(dd_dat_groups)){
      if(dd_dat_groups$binomial[i] %in% unique(shapes$binomial)){
        dd_dat_groups$groups[i]<-nam[d]  
      }
    }
    
  }
  dd_dat_groups2<-rbind(dd_dat_groups2,subset(dd_dat_groups, dd_dat_groups$groups==nam[d]))
}

dd_dat_groups3<-subset(dd_dat_groups2, dd_dat_groups2$yrcompiled>=2010)

dd_dat_groups3$category_group<-factor(dd_dat_groups3$category_group, 
                                      levels=c("threatened", "not threatened", "DD"),
                                      labels = c("Threatened species", "Not threatened species",
                                                 "Data-Deficient species"))

dd_dat_groups3$category<-factor(dd_dat_groups3$category, 
                                levels=c("LC", "NT", "VU", "EN", "CR", "DD"),
                                labels = c("Least Concern", "Near Threatened",
                                           "Vulnerable", "Endangered",
                                           "Critically Endangered",
                                           "Data Deficient"))

p<-ggplot(dd_dat_groups3, aes(x = threatened, y = groups, fill = category)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.9, direction = -1, option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

#
ggsave(p, file="C:/Users/janbor/Desktop/output_densityplot.png", width=10, height=15, dpi=900)
#

dd_dat_groups4<-subset(dd_dat_groups3, dd_dat_groups3$groups=="MARINEGROUPS")

dd_dat_groups4<-subset(dd_dat_groups4, dd_dat_groups4$class=="GASTROPODA")
dd_dat_groups4<-subset(dd_dat_groups3, dd_dat_groups3$order_=="NEOGASTROPODA")

p<-ggplot(dd_dat_groups4, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.9, direction = -1, option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

(sort(table(dd_dat_groups4$category)))
#




load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/ens_pred_dd")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/df_ml1")

dd_dat<-cbind(df_ml1, ens_pred_dd)
dd_dat<-subset(dd_dat, dd_dat$category!="EX")
dd_dat<-subset(dd_dat, dd_dat$category!="EW")



dd_dat<-cbind(preds_all)
dd_dat<-subset(dd_dat, dd_dat$category!="EX")
dd_dat<-subset(dd_dat, dd_dat$category!="EW")
dd_dat_mar<-subset(dd_dat,tolower(dd_dat$marine)=="true")
length(which(dd_dat_mar$needs_update=="No"))


plot(log(dd_dat_mar$range_extent), dd_dat_mar$threatened)


dd_dat_mar$systems<-"M"
dd_dat_mar$systems[which(tolower(dd_dat_mar$freshwater)=="true" & 
                           tolower(dd_dat_mar$terrestial)=="true")] <- "MFT"
dd_dat_mar$systems[which(tolower(dd_dat_mar$freshwater)=="true" & 
                           tolower(dd_dat_mar$terrestial)=="false")] <- "MF"
dd_dat_mar$systems[which(tolower(dd_dat_mar$freshwater)=="false" & 
                           tolower(dd_dat_mar$terrestial)=="true")] <- "MT"


p<-ggplot(dd_dat_mar, aes(x = threatened, y = systems, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.5, direction = -1, option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p
table(dd_dat_mar$class)
table(dd_dat_mar$category)
table(dd_dat_mar$systems)


#########



dd_dat$systems<-NA
dd_dat$systems[which(tolower(dd_dat$marine)=="true" & 
                       tolower(dd_dat$freshwater)=="true" &
                       tolower(dd_dat$terrestial)=="true")] <- "MFT"
dd_dat$systems[which(tolower(dd_dat$marine)=="true" & 
                       tolower(dd_dat$freshwater)=="true" &
                       tolower(dd_dat$terrestial)=="false")] <- "MF"
dd_dat$systems[which(tolower(dd_dat$marine)=="true" & 
                       tolower(dd_dat$freshwater)=="false" &
                       tolower(dd_dat$terrestial)=="true")] <- "MT"
dd_dat$systems[which(tolower(dd_dat$marine)=="true" & 
                       tolower(dd_dat$freshwater)=="false" &
                       tolower(dd_dat$terrestial)=="false")] <- "M"
dd_dat$systems[which(tolower(dd_dat$marine)=="false" & 
                       tolower(dd_dat$freshwater)=="true" &
                       tolower(dd_dat$terrestial)=="true")] <- "FT"
dd_dat$systems[which(tolower(dd_dat$marine)=="false" & 
                       tolower(dd_dat$freshwater)=="true" &
                       tolower(dd_dat$terrestial)=="false")] <- "F"
dd_dat$systems[which(tolower(dd_dat$marine)=="false" & 
                       tolower(dd_dat$freshwater)=="false" &
                       tolower(dd_dat$terrestial)=="true")] <- "T"



p<-ggplot(dd_dat, aes(x = threatened, y = systems, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.5, direction = -1, option = "B") +
  xlab("Predicted Extinction Risk") +
  ylab("IUCN Dataset") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

table(dd_dat$systems)


##


































for(d in 1:length(nam)){
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_all_risk",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_dd_risk",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_all_n",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_dd_n",sep=""))
  #load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_all_threatened",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_dd_threatened",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_threatened_n",sep=""))
  
  par(mfrow=c(3,2))
  par(mar=c(0,4,2,0))
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Number of DD species", legend=F)
  plot(dd_n, col=viridis(64, direction = -1),box=F,axes=F,xaxt="n", yaxt="n", add=T)
  mtext(paste(nam[d]), padj=0, adj=0)
  
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n", ylab="average DD extinction risk", legend=F)
  plot(dd_risk/dd_n, col=colfunc1(64),xaxt="n", yaxt="n", box=F,axes=F,add=T)
  
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Potentially threatened DD species", legend=F)
  plot(dd_threatened, col=colfunc1(64),box=F,axes=F,xaxt="n", yaxt="n", add=T)
  
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="DD contribution to total extinction risk", legend=F)
  plot(dd_risk/(all_risk), col=colfunc1(64),box=F,axes=F,xaxt="n", yaxt="n", add=T)
  
  dif<-((threatened_n+dd_threatened)/all_n)-((threatened_n)/(all_n-dd_n))
  dif[dif>0.1]<-0.1
  dif[dif<(-0.1)]<-(-0.1)
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Difference in fraction of threatened species between DD and non DD", legend=F)
  plot(dif, col=colfunc2(64),box=F,axes=F,xaxt="n", yaxt="n", add=T, zlim=c(-0.1,0.1))
  
  
  dif<-(all_risk/all_n)-((all_risk-dd_risk)/(all_n-dd_n))
  dif[dif>0.1]<-0.1
  dif[dif<(-0.1)]<-(-0.1)
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Difference in average extinction risk DD and non DD", legend=F)
  plot(dif, col=colfunc2(64),box=F,axes=F,xaxt="n", yaxt="n", add=T, zlim=c(-0.1,0.1))
  
  
  par(mfrow=c(1,1))
}






darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=2)
darkcols2<-brewer.pal(11, "RdYlBu")
darkcols2<-rev(darkcols2)
colfunc2<-colorRampPalette(darkcols2, bias=1)


for(d in 1:length(nam)){
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_all_risk",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_dd_risk",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_all_n",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_dd_n",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_all_threatened",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_dd_threatened",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/",nam[d],"/",nam[d],"_threatened_n",sep=""))
  
  par(mfrow=c(2,2))
  plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n",ylab="total extinction risk", legend=F)
  plot(all_risk, col=colfunc1(64),xaxt="n", yaxt="n", add=T)
  plot(coast_lines, col="grey", add=T)
  mtext(paste(nam[d]), padj=0, adj=0)
  
  
  contr_dd<-(dd_risk/all_risk)
  contr_dd[is.na(contr_dd)]<-0
  
  plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n",ylab="DD contribution to total extinction risk", legend=F)
  plot(contr_dd, col=colfunc1(64),xaxt="n", yaxt="n", add=T)
  plot(coast_lines, col="grey", add=T)
  
  avg_risk_dd<-dd_risk/dd_n
  avg_risk_dd[is.na(avg_risk_dd)]<-0
  
  plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n", ylab="average extinction risk of DD species", legend=F)
  plot(avg_risk_dd, col=colfunc1(64),xaxt="n", yaxt="n", add=T, zlim=c(0,1))
  plot(coast_lines, col="grey", add=T)
  
  
  dif<-(dd_risk/dd_n)-((all_risk-dd_risk)/(all_n-dd_n))
  dif[is.na(dif)]<-0
  
  dif[dif>0.1]<-0.1
  dif[dif<(-0.1)]<-(-0.1)
  
  
  plot(all_risk, col=darkcols2[round(length(darkcols2)/2)], xaxt="n", yaxt="n",ylab="Difference in average extinction risk to non-DD", legend=F)
  plot(dif, col=colfunc2(64),xaxt="n", yaxt="n", add=T, zlim=c(-0.1,0.1))
  plot(coast_lines, col="grey", add=T)
  
  
  par(mfrow=c(1,1))
}

par(mfrow=c(2,2))

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n",ylab="total extinction risk", legend=F)
plot(all_risk_global, col=colfunc1(64),xaxt="n", yaxt="n", add=T)
plot(coast_lines, col="grey", add=T)
mtext("Global", padj=0, adj=0)

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n",ylab="DD contribution to total extinction risk", legend=F)
plot(contr_dd_global, col=colfunc1(64),xaxt="n", yaxt="n", add=T)
plot(coast_lines, col="grey", add=T)

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n", ylab="average extinction risk of DD species", legend=F)
plot(avg_risk_dd_global, col=colfunc1(64),xaxt="n", yaxt="n", add=T, zlim=c(0,1))
plot(coast_lines, col="grey", add=T)

plot(all_risk, col=darkcols2[round(length(darkcols2)/2)], xaxt="n", yaxt="n",ylab="Difference in average extinction risk to non-DD", legend=F)
plot(dif_global, col=colfunc2(64),xaxt="n", yaxt="n", add=T, zlim=c(-0.1,0.1))
plot(coast_lines, col="grey", add=T)

par(mfrow=c(1,1))
#


#

#













library(raster)
library(viridis)
library(RColorBrewer)
darkcols1<-inferno(10)[1:10]
colfunc1<-colorRampPalette(darkcols1, bias=2)

darkcols2<-brewer.pal(11, "RdYlBu")
darkcols2<-rev(darkcols2)
colfunc2<-colorRampPalette(darkcols2, bias=1)

for(d in 1){
  
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_risk_global",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_risk_global",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_n_global",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_n_global",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/all_threatened_global",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/dd_threatened_global",sep=""))
  load(file=paste("C:/Users/janbor/Desktop/OneDrive - NTNU/Data/iucn_gbif/ML/predictions/GLOBAL/threatened_n_global",sep=""))
  
  
  par(mfrow=c(3,2))
  par(mar=c(0,4,2,0))
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Number of DD species", legend=F)
  plot(dd_n, col=viridis(64, direction = -1),box=F,axes=F,xaxt="n", yaxt="n", add=T)
  mtext("GLOBAL", padj=0, adj=0)
  
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n", ylab="average DD extinction risk", legend=F)
  plot(dd_risk/dd_n, col=colfunc1(64),xaxt="n", yaxt="n", box=F,axes=F,add=T)
  
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Potentially threatened DD species", legend=F)
  plot(dd_threatened, col=colfunc1(64),box=F,axes=F,xaxt="n", yaxt="n", add=T)
  
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="DD contribution to total extinction risk", legend=F)
  plot(dd_risk/(all_risk), col=colfunc1(64),box=F,axes=F,xaxt="n", yaxt="n", add=T)
  
  dif<-((threatened_n+dd_threatened)/all_n)-((threatened_n)/(all_n-dd_n))
  dif[dif>0.1]<-0.1
  dif[dif<(-0.1)]<-(-0.1)
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Difference in fraction of threatened species between DD and non DD", legend=F)
  plot(dif, col=colfunc2(64),box=F,axes=F,xaxt="n", yaxt="n", add=T, zlim=c(-0.1,0.1))
  
  
  dif<-(all_risk/all_n)-((all_risk-dd_risk)/(all_n-dd_n))
  dif[dif>0.1]<-0.1
  dif[dif<(-0.1)]<-(-0.1)
  
  plot(all_risk, col=inferno(10, alpha = 0.8)[1],box=F,axes=F, xaxt="n", yaxt="n",ylab="Difference in average extinction risk DD and non DD", legend=F)
  plot(dif, col=colfunc2(64),box=F,axes=F,xaxt="n", yaxt="n", add=T, zlim=c(-0.1,0.1))
  
  
  par(mfrow=c(1,1))
}

par(mfrow=c(2,2))

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n",ylab="total extinction risk", legend=F)
plot(all_risk, col=colfunc1(64),xaxt="n", yaxt="n", add=T)
plot(coast_lines, col="grey", add=T)
mtext("GLOBAL", padj=0, adj=0)

contr_dd<-(dd_risk/all_risk)
contr_dd[is.na(contr_dd)]<-0

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n",ylab="DD contribution to total extinction risk", legend=F)
plot(contr_dd, col=colfunc1(64),xaxt="n", yaxt="n", add=T, zlim=c(0,0.2))
plot(coast_lines, col="grey", add=T)

avg_risk_dd<-dd_risk/dd_n
avg_risk_dd[is.na(avg_risk_dd)]<-0

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n", ylab="average extinction risk of DD species", legend=F)
plot(avg_risk_dd, col=colfunc1(64),xaxt="n", yaxt="n", add=T, zlim=c(0,1))
plot(coast_lines, col="grey", add=T)


dif<-(dd_risk/dd_n)-((all_risk-dd_risk)/(all_n-dd_n))
dif[is.na(dif)]<-0

plot(all_risk, col=darkcols2[round(length(darkcols2)/2)], xaxt="n", yaxt="n",ylab="Difference in average extinction risk to non-DD", legend=F)
plot(dif, col=colfunc2(64),xaxt="n", yaxt="n", add=T, zlim=c(-0.1,0.1))
plot(coast_lines, col="grey", add=T)


par(mfrow=c(1,1))
#

dd_fract<-dd_threatened/dd_n
#dd_fract[dd_fract>0.5]<-0.5
plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n", ylab="average extinction risk of DD species", legend=F)
plot(dd_fract, col=colfunc1(64), add=T, zlim=c(0,0.5))


plot(dd_n, col="black", legend=F)
plot(log(dd_n), col=viridis(64, direction = 1), add=T)

dd_dif<-(dd_threatened/dd_n)-(threatened_n/(all_n-dd_n))
dd_dif[dd_dif>0.1]<-0.1
dd_dif[dd_dif<(-0.1)]<-(-0.1)

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n", ylab="average extinction risk of DD species", legend=F)
plot(dd_dif, col=colfunc2(64), add=T, zlim=c(-0.1,0.1))



dd_dif_risk<-(dd_risk/dd_n)-((all_risk-dd_risk)/(all_n-dd_n))
dd_dif_risk[dd_dif_risk>0.1]<-0.1
dd_dif_risk[dd_dif_risk<(-0.1)]<-(-0.1)

plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n", ylab="average extinction risk of DD species", legend=F)
plot(dd_dif_risk, col=colfunc2(64), add=T, zlim=c(-0.1,0.1))


plot(all_risk, col=inferno(10, alpha = 0.9)[1], xaxt="n", yaxt="n", ylab="average extinction risk of DD species", legend=F)
plot(dd_risk, col=colfunc1(64), add=T)

#
#################################################
library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(raster)
library(rasterVis)
library(classInt)
library(RColorBrewer)
# Natural Earth shape files -- global (Robinson) projections
# get shapefiles from http://www.naturalearthdata.com



shape_path <- "C:/Users/janbor/Desktop/OneDrive - NTNU/Data/natural_earth_vector/"


coast_shapefile <- paste(shape_path, "50m_physical/ne_50m_coastline.shp", sep="")
ocean_shapefile <- paste(shape_path, "50m_physical/ne_50m_ocean.shp", sep="")
admin0_shapefile <- paste(shape_path, "50m_cultural/ne_50m_admin_0_countries.shp", sep="")
admin1_shapefile <- paste(shape_path, "50m_cultural/ne_50m_admin_1_states_provinces_lakes.shp", sep="")
lakes_shapefile <- paste(shape_path, "50m_physical/ne_50m_lakes.shp", sep="")
bb_shapefile <- paste(shape_path, "50m_physical/ne_50m_graticules_all/ne_50m_wgs84_bounding_box.shp", sep="")
grat30_shapefile <- paste(shape_path, "50m_physical/ne_50m_graticules_all/ne_50m_graticules_30.shp", sep="")


# find out kind of shapefile (lines vs. polygons)
layer <- ogrListLayers(coast_shapefile)
# read the shape file
coast_lines <- readOGR(coast_shapefile, layer=layer)

unproj_proj4string <- proj4string(coast_lines)
unproj_proj4string


# find out kind of shapefile (lines vs. polygons)
layer <- ogrListLayers(ocean_shapefile)
# read the shape file
ocean_poly <- readOGR(ocean_shapefile, layer=layer)

layer <- ogrListLayers(admin0_shapefile)
admin0_poly <- readOGR(admin0_shapefile, layer=layer)

layer <- ogrListLayers(admin1_shapefile)
admin1_poly <- readOGR(admin1_shapefile, layer=layer)

layer <- ogrListLayers(lakes_shapefile)
lakes_poly <- readOGR(lakes_shapefile, layer=layer)

lakes_poly$scalerank <- as.numeric(lakes_poly$scalerank)
lrglakes_poly <- subset(lakes_poly, lakes_poly$scalerank <= 2)

layer <- ogrListLayers(grat30_shapefile)
grat30_lines <- readOGR(grat30_shapefile, layer=layer)

layer <- ogrListLayers(bb_shapefile)
bb_poly <- readOGR(bb_shapefile, layer=layer)
bb_lines <- as(bb_poly, "SpatialLines")


# set Robinson CRS
unproj_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
robin_crs <- CRS("+proj=robin +lon_0=0w")
# define projections
mollCRS <- CRS('+proj=moll')
behrmannCRS <- CRS('+proj=cea +lat_ts=30')


#kaspian see missing
caspian_bb <- as(extent(45, 56, 35, 50), "SpatialPolygons")
proj4string(caspian_bb) <- unproj_proj4string
caspian_poly <- gIntersection(ocean_poly, caspian_bb)
proj4string(caspian_poly) <- unproj_proj4string
caspian_poly_proj <- spTransform(caspian_poly, robin_crs)

# do other projections
bb_poly_proj <- spTransform(bb_poly, robin_crs)
coast_lines_proj <- spTransform(coast_lines, robin_crs)
admin0_poly_proj <- spTransform(admin0_poly, robin_crs)
admin1_poly_proj <- spTransform(admin1_poly, robin_crs)
lakes_poly_proj <- spTransform(lakes_poly, robin_crs)
grat30_lines_proj <- spTransform(grat30_lines, robin_crs)
lrglakes_poly_proj <- spTransform(lrglakes_poly, robin_crs)
ocean_poly_proj <- spTransform(ocean_poly, robin_crs)


# convert polygons to spatial lines
admin0_lines_proj <- as(admin0_poly_proj, "SpatialLines")
bb_lines_proj <- as(bb_poly_proj, "SpatialLines")

# set Robinson CRS
unproj_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
robin_crs <- CRS("+proj=robin +lon_0=0w")
# define projections
mollCRS <- CRS('+proj=moll')
behrmannCRS <- CRS('+proj=cea +lat_ts=30')




###############################################################


bg_rob <- projectRaster(all_risk, crs=robin_crs)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_maps.png"
png(pngfile, width=32, height=22, units = "cm", res=900)

opar<-par()
par(mfrow=c(2,2))
par(mar=c(3,0,0,0))

sst_longlat<-all_risk
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=inferno(10, alpha = 0.8)[1], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=inferno(10, alpha = 0.8)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(64), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(64), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,maxValue(sst_rob),maxValue(sst_rob)),
                    labels=c("Low","High"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Predicted extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)

###########
sst_longlat<-contr_dd
sst_longlat[sst_longlat>0.2]<-0.2
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=inferno(10, alpha = 0.8)[1], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=inferno(10, alpha = 0.8)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(64), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(64), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%",
                             "10%","15%","> 20%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Contribution of Data-Deficient species to predicted extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)
########

sst_longlat<-avg_risk_dd
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=inferno(10, alpha = 0.8)[1], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=inferno(10, alpha = 0.8)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(64), legend=F, add=TRUE, axes=F, zlim=c(0,1))
plot(sst_rob, legend.only=TRUE, col=colfunc1(64),zlim=c(0,1), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,1,0.2),
                    labels=c("0%","20%",
                             "40%","60%","80%","100%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Average predicted extinction risk of Data-Deficient species", side=1, adj = 0.1, padj = -3, cex=0.8)

################


sst_longlat<-dif
sst_longlat[sst_longlat>0.1]<-0.1
sst_longlat[sst_longlat<(-0.1)]<-(-0.1)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=darkcols2[round(length(darkcols2)/2)], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=darkcols2[round(length(darkcols2)/2)], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc2(64), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc2(64), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(-0.1,0.1,0.05),
                    labels=c("< -10%","-5%",
                             "0%","5%","> 10%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Difference average predicted extinction risk between Data-Deficient and non Data-Deficient species", side=1, adj = 0.1, padj = -3, cex=0.8)


par(mfrow=c(1,1))

par(opar)
dev.off()




############################################





bg_rob <- projectRaster(all_risk, crs=robin_crs)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/output_maps2.png"
png(pngfile, width=32, height=22, units = "cm", res=900)

opar<-par()
par(mfrow=c(2,2))
par(mar=c(3,0,0,0))

sst_longlat<-all_risk
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=inferno(10, alpha = 0.8)[1], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=inferno(10, alpha = 0.8)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(64), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(64), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,maxValue(sst_rob),maxValue(sst_rob)),
                    labels=c("Low","High"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Predicted extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)

###########
sst_longlat<-contr_dd
sst_longlat[sst_longlat>0.2]<-0.2
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=inferno(10, alpha = 0.8)[1], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=inferno(10, alpha = 0.8)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(64), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc1(64), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%",
                             "10%","15%","> 20%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Contribution of Data-Deficient species to predicted extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)
########

sst_longlat<-avg_risk_dd
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=inferno(10, alpha = 0.8)[1], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=inferno(10, alpha = 0.8)[1], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc1(64), legend=F, add=TRUE, axes=F, zlim=c(0,1))
plot(sst_rob, legend.only=TRUE, col=colfunc1(64),zlim=c(0,1), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,1,0.2),
                    labels=c("0%","20%",
                             "40%","60%","80%","100%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)
mtext("Average predicted extinction risk of Data-Deficient species", side=1, adj = 0.1, padj = -3, cex=0.8)

################


sst_longlat<-dif
sst_longlat[sst_longlat>0.1]<-0.1
sst_longlat[sst_longlat<(-0.1)]<-(-0.1)

sst_rob <- projectRaster(sst_longlat, crs=robin_crs)

plot(bb_poly_proj, col=darkcols2[round(length(darkcols2)/2)], bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bg_rob, col=darkcols2[round(length(darkcols2)/2)], legend=F, add=TRUE, axes=F)
plot(sst_rob, col=colfunc2(64), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=colfunc2(64), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(-0.1,0.1,0.05),
                    labels=c("< -10%","-5%",
                             "0%","5%","> 10%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="darkgray", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Difference average predicted extinction risk between Data-Deficient and non Data-Deficient species", side=1, adj = 0.1, padj = -3, cex=0.8)


par(mfrow=c(1,1))

par(opar)
dev.off()






##################################



#distribution of most threatened DD species (should be prioritized)








##

# library
library(ggridges)
library(ggplot2)
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/Version2/dataframes/complete/preds_all2")
#load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/Version2/dataframes/complete/df_ml1")

length(unique(preds_all$binomial))
preds_all$count<-1
preds_all<-subset(preds_all, preds_all$category!="EX")
preds_all<-subset(preds_all, preds_all$category!="EW")
preds_all<-subset(preds_all, preds_all$category!="DD")
#preds_all<-subset(preds_all, preds_all$yrcompiled>=2010)

rl_needs_update<-read.csv("X:/indecol/USERS/JanB/Data/iucn_gbif/ML/rl_needs_update/assessments.csv")
preds_all$needs_update<-"No"
for(i in 1:nrow(preds_all)){
  if(preds_all$binomial[i] %in% rl_needs_update$scientificName){
    preds_all$needs_update[i]<-"Yes"  
  }
}
preds_all<-subset(preds_all, preds_all$needs_update=="No")

for(i in 3:8){
  if(length(levels(preds_all[,i])) != length(unique(preds_all[,i]))){
    print(colnames(preds_all[i]))  
  }
}

#load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/ens_pred_dd")
#load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/df_ml1")


dd_dat<-cbind(preds_all)
dd_dat<-subset(dd_dat, dd_dat$category!="EX")
dd_dat<-subset(dd_dat, dd_dat$category!="EW")
#dd_dat<-subset(dd_dat, dd_dat$yrcompiled>=2010)
#dd_dat1<-dd_dat

dd_dat$category_dd<-NA
for(i in 1:nrow(dd_dat)){
  if(dd_dat$category[i]=="DD"){
    dd_dat$category_dd[i]<-"Data-Deficient Species"
  } else {
    dd_dat$category_dd[i]<-"non Data-Deficient Species"
  }
}

dd_dat$class<-as.character(dd_dat$class)
dd_dat2<-subset(dd_dat, dd_dat$class!="LECANOROMYCETES")
dd_dat2<-subset(dd_dat2, dd_dat2$class!="JUNGERMANNIOPSIDA")
dd_dat2<-subset(dd_dat2, dd_dat2$category!="EX")
dd_dat2<-subset(dd_dat2, dd_dat2$category!="EW")
dd_dat2$class<-factor(as.character(dd_dat2$class),
                      levels = sort(unique(dd_dat2$class)))

# basic example
p<-ggplot(dd_dat2, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.85) +
  xlab("Predicted Extinction Risk") +
  ylab("Frequency") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

#
ggsave(p, file="C:/Users/janbor/Desktop/output_densityplot.png", width=10, height=15, dpi=900)
#

dd_dat_new<-subset(dd_dat, dd_dat$yrcompiled>=2010)
#dd_dat_new<-subset(dd_dat_new, tolower(dd_dat_new$marine)=="true")
dd_dat_new$category_group<-factor(dd_dat_new$category_group, levels = c("DD","not threatened","threatened"))
p<-ggplot(dd_dat_new, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.85, direction = -1) +
  #scale_color_manual(values = c(viridis(3)[3],viridis(3)[2],viridis(3)[1])) +
  xlab("Predicted Extinction Risk") +
  ylab("Assessments (after 2010)") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p
nrow(subset(dd_dat_new, dd_dat_new$class=="ANTHOZOA"))

####
dd_dat_new<-subset(dd_dat, dd_dat$yrcompiled>=2010)

dd_dat_new$system<-NA
dd_dat_new_terr<-subset(dd_dat_new, tolower(dd_dat_new$terrestial)=="true")
dd_dat_new_terr$system<-"Terrestrial Species"

dd_dat_new_fresh<-subset(dd_dat_new, tolower(dd_dat_new$freshwater)=="true")
dd_dat_new_fresh$system<-"Freshwater Species"

dd_dat_new_mar<-subset(dd_dat_new, tolower(dd_dat_new$marine)=="true")
dd_dat_new_mar$system<-"Marine Species"

dd_dat_new_system<-rbind(dd_dat_new_terr, dd_dat_new_fresh, dd_dat_new_mar)

p<-ggplot(dd_dat_new_system, aes(x = threatened, y = system, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.7) +
  xlab("Predicted Extinction Risk") +
  ylab("Assessments (after 2010)") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

#



## needs update
#dd_dat_old<-subset(dd_dat, dd_dat$yrcompiled<2010)
dd_dat_old<-dd_dat
dd_dat_old<-subset(dd_dat_old, dd_dat_old$category!="EW")
dd_dat_old<-subset(dd_dat_old, dd_dat_old$category!="EX")

dd_dat_old$needs_update<-"No"
for(i in 1:nrow(dd_dat_old)){
  if(dd_dat_old$binomial[i] %in% rl_needs_update$scientificName){
    dd_dat_old$needs_update[i]<-"Yes"  
  }
}

dd_dat_old$system<-NA
dd_dat_old_terr<-subset(dd_dat_old, tolower(dd_dat_old$terrestial)=="true")
dd_dat_old_terr$system<-"Terrestrial Species"

dd_dat_old_fresh<-subset(dd_dat_old, tolower(dd_dat_old$freshwater)=="true")
dd_dat_old_fresh$system<-"Freshwater Species"

dd_dat_old_mar<-subset(dd_dat_old, tolower(dd_dat_old$marine)=="true")
dd_dat_old_mar$system<-"Marine Species"

dd_dat_old_system<-rbind(dd_dat_old_terr, dd_dat_old_fresh, dd_dat_old_mar)



p<-ggplot(dd_dat_old_system, aes(x = threatened, y = system, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.7) +
  xlab("Predicted Extinction Risk") +
  ylab("Needs Update") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p
#
dd_dat_ndd<-subset(dd_dat, dd_dat$category_group!="DD")
nrow(subset(dd_dat_ndd, dd_dat_ndd$category_group=="threatened"))/nrow(dd_dat_ndd)


dd_dat_dd<-subset(dd_dat, dd_dat$category_group=="DD")
nrow(subset(dd_dat_dd, dd_dat_dd$predict=="threatened"))/nrow(dd_dat_dd)

###

rl_needs_update<-read.csv("C:/Users/janbor/Desktop/rl_needs_update/assessments.csv")
dd_dat_update<-dd_dat
dd_dat_update<-subset(dd_dat_update, dd_dat_update$category!="EW")
dd_dat_update<-subset(dd_dat_update, dd_dat_update$category!="EX")

dd_dat_update$needs_update<-"No"
for(i in 1:nrow(dd_dat_update)){
  if(dd_dat_update$binomial[i] %in% rl_needs_update$scientificName){
    dd_dat_update$needs_update[i]<-"Yes"  
  }
}

dd_dat_update<-subset(dd_dat_update, tolower(dd_dat_update$class)!="anthozoa")
p<-ggplot(dd_dat_update, aes(x = threatened, y = needs_update, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.7) +
  xlab("Predicted Extinction Risk") +
  ylab("Needs update") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

##

######

dd_dat_groups<-dd_dat
dd_dat_groups<-subset(dd_dat_groups, dd_dat_groups$category!="EW")
dd_dat_groups<-subset(dd_dat_groups, dd_dat_groups$category!="EX")

dd_dat_groups$groups<-NA
dd_dat_groups2<-data.frame()
for(d in 1:length(dirs)){
  shp_files<-list.files(paste(dirs[d],sep=""), pattern = ".shp$", recursive = T, full.names = T)
  print(d)
  for(a in 1:length(shp_files)){
    
    shapes <- st_read(shp_files[a])
    for(i in 1:nrow(dd_dat_groups)){
      if(dd_dat_groups$binomial[i] %in% unique(shapes$binomial)){
        dd_dat_groups$groups[i]<-nam[d]  
      }
    }
    
  }
  dd_dat_groups2<-rbind(dd_dat_groups2,subset(dd_dat_groups, dd_dat_groups$groups==nam[d]))
}

dd_dat_groups3<-subset(dd_dat_groups2, dd_dat_groups2$yrcompiled>=2010)
p<-ggplot(dd_dat, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.7) +
  xlab("Predicted Extinction Risk") +
  ylab("Dataset (>2010)") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p




nrow(subset(dd_dat_groups2,dd_dat_groups2$groups=="FISHES"))
#
dd_dat_groups2_mar<-subset(dd_dat_groups2, dd_dat_groups2$groups=="FISHES")
p<-ggplot(dd_dat_groups2_mar, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.7) +
  xlab("Predicted Extinction Risk") +
  ylab("Marine groups") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$class=="LILIOPSIDA"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$class=="HYDROZOA"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$class=="ANTHOZOA"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$class=="GASTROPODA"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$class=="MAGNOLIOPSIDA"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$class=="MALACOSTRACA"))

nrow(dd_dat_groups2_mar)
#
#
dd_dat_groups2_mar<-subset(dd_dat_groups2, dd_dat_groups2$groups=="FISHES")
dd_dat_groups2_mar<-subset(dd_dat_groups2_mar, dd_dat_groups2_mar$order_=="PERCIFORMES")
#dd_dat_groups2_mar<-subset(dd_dat_groups2_mar, dd_dat_groups2_mar$yrcompiled>=2010)

p<-ggplot(dd_dat_groups2_mar, aes(x = threatened, y = order_, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.5) +
  xlab("Predicted Extinction Risk") +
  ylab("Fish families") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$order_=="PERCIFORMES"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$order_=="SYNGNATHIFORMES"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$order_=="CLUPEIFORMES"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$family=="EPINEPHELIDAE"))

dd_dat_groups2_mar$count<-1
aggregate(dd_dat_groups2_mar$count~dd_dat_groups2_mar$order_, FUN=sum)
nrow(dd_dat_groups2_mar)

nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$yrcompiled<2010))/nrow(dd_dat_groups2_mar)

#
dd_dat_groups2_mar<-dd_dat_new_mar
dd_dat_groups2_mar<-subset(dd_dat_groups2_mar, dd_dat_groups2_mar$class=="ACTINOPTERYGII")
dd_dat_groups2_mar<-subset(dd_dat_groups2_mar, dd_dat_groups2_mar$yrcompiled>=2010)

p<-ggplot(dd_dat_groups2_mar, aes(x = threatened, y = class, fill = category_group)) +
  geom_density_ridges() +
  scale_fill_viridis_d(2, alpha = 0.7) +
  xlab("Predicted Extinction Risk") +
  ylab("Marine groups") +
  xlim(0,1) +
  theme_ridges() + 
  theme(legend.position = "top", legend.title = element_blank())
p

dd_dat_groups2_mar$count<-1
aggregate(dd_dat_groups2_mar$count~dd_dat_groups2_mar$class, FUN=sum)



nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$order_=="PERCIFORMES"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$order_=="SYNGNATHIFORMES"))
nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$order_=="CLUPEIFORMES"))

nrow(dd_dat_groups2_mar)

nrow(subset(dd_dat_groups2_mar,dd_dat_groups2_mar$yrcompiled<2010))/nrow(dd_dat_groups2_mar)
#






########

library(raster)
library(viridis)

load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/mar_all")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/mar_threatened")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/mar_richness")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/terr_all")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/terr_threatened")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/terr_richness")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/fresh_all")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/fresh_threatened")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/fresh_richness")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/mar_dd_all")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/mar_dd_threatened")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/mar_dd_richness")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/terr_dd_all")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/terr_dd_threatened")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/terr_dd_richness")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/fresh_dd_all")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/fresh_dd_threatened")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/fresh_dd_richness")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/global_all")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/global_threatened")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/global_richness")


load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/ens_pred_dd")
load(file="X:/indecol/USERS/JanB/Data/iucn_gbif/ML/predictions/df_ml1")

dd_dat<-cbind(df_ml1[c(2,18, 22:25)], ens_pred_dd)
dd_dat<-subset(dd_dat, dd_dat$category!="EX")
dd_dat<-subset(dd_dat, dd_dat$category!="EW")


df_ml1_noDD<-subset(df_ml1, df_ml1$category!="DD")
length(unique(df_ml1_noDD$binomial))




library(raster)
plot(global_richness, col=viridis(64))
plot(global_richness/global_all, col=viridis(64))

global_threatened<-terr_threatened+mar_threatened+fresh_threatened
global_all<-terr_all+mar_all+fresh_all
global_fraction<-global_threatened/global_all
maxValue(global_fraction)
global_fraction[global_fraction>0.2]<-0.2

plot(global_fraction, col=inferno(64))

terr_fraction<-terr_dd_richness/terr_richness
terr_fraction[is.na(globe)]<-NA
terr_fraction[terr_fraction>0.3]<-0.3
plot(terr_fraction, col=inferno(64))

fresh_fraction<-fresh_dd_richness/fresh_richness
fresh_fraction[is.na(globe)]<-NA
fresh_fraction[fresh_fraction>0.3]<-0.3
plot(fresh_fraction, col=inferno(64))

mar_fraction<-mar_dd_richness/mar_richness
mar_fraction[!is.na(globe)]<-NA
mar_fraction[mar_fraction>0.3]<-0.3
plot(mar_fraction, col=inferno(64))

all_fraction<-mean(terr_fraction,fresh_fraction)
all_fraction[is.na(all_fraction)]<-0
plot(all_fraction, col=inferno(64))

mar_fraction[is.na(mar_fraction)]<-0
mar_fraction <- (mar_fraction*(-1))
all_fraction<-all_fraction+mar_fraction
plot(mar_fraction)

library(wesanderson)
library(RColorBrewer)

darkcols1<-c(wes_palette("Zissou1",n=5))

darkcols1<-rev(brewer.pal(n = 8, name = "RdBu"))
colfunc1<-colorRampPalette(darkcols1, bias=1)

plot(all_fraction, col=colfunc1(64))


#






global_map <- shapefile("X:/indecol/USERS/JanB/Data/iucn_gbif/global_map.shp")
global_map<-spTransform(global_map,CRS("+proj=longlat +datum=WGS84"))

globe<-rasterize(global_map, terr_all)
mar_richness1<-mar_richness
mar_richness1[globe==1]<-NA

mar_all1<-mar_all
mar_all1[globe==1]<-NA


mar_dd_richness1<-mar_dd_richness
mar_dd_richness1[globe==1]<-NA


terr_richness1<-mask(terr_richness, global_map)

fresh_richness1<-mask(fresh_richness, global_map)
fresh_dd_richness1<-mask(fresh_dd_richness, global_map)

terr_dd_richness1<-mask(terr_dd_richness, global_map)

fresh_all1<-mask(fresh_all, global_map)
terr_all1<-mask(terr_all, global_map)



terr_threatened1<-mask(terr_threatened, global_map)
terr_dd_threatened1<-mask(terr_dd_threatened, global_map)

fresh_dd_threatened1<-mask(fresh_dd_threatened, global_map)


dd_fraction_fresh<-fresh_dd_richness1/fresh_richness
dd_fraction_fresh[dd_fraction_fresh>0.2]<-0.2

plot(dd_fraction_fresh, col=inferno(64, direction=1))
plot(global_map, add=T)

dd_fraction_terr<-terr_dd_richness1/terr_richness
#density(dd_fraction_terr)
#maxValue(dd_fraction_terr)
dd_fraction_terr[dd_fraction_terr>0.2]<-0.2

plot(dd_fraction_terr, col=inferno(64, direction=1))
plot(global_map, add=T)


dd_fraction_mar<-mar_dd_richness/mar_richness1
#density(dd_fraction_mar)
dd_fraction_mar[dd_fraction_mar>0.2]<-0.2

plot(dd_fraction_mar, col=inferno(64, direction=1))
plot(global_map, col="black", add=T)




average_mar<-mar_dd_threatened/mar_all
average_mar[average_mar>0.2]<-0.2
plot(average_mar, col=inferno(64, direction=1))

terr_dd_richness1<-mask(terr_dd_richness, global_map)

fresh_dd_richness1<-mask(fresh_dd_richness, global_map)
fresh_dd_richness1[fresh_dd_richness1>1]<-1

plot(log(mar_dd_richness1+1), col=inferno(64, direction=1))
plot(log(fresh_dd_richness1+1), col=inferno(64, direction=1))

plot(terr_dd_all, col=inferno(64, direction=1))


plot(mar_dd_all/mar_all)
plot(mar_dd_all)




plot(log(fresh_all), col=inferno(64, direction=1))
plot(log(terr_all), col=inferno(64, direction=1))
plot(log(mar_all), col=inferno(64, direction=1))
#





df_ml1_terr<-subset(df_ml1, tolower(df_ml1$terrestial)=="true")
df_ml1_terr_dd<-subset(df_ml1_terr, df_ml1_terr$category=="DD")


length(unique(df_ml1_terr_dd$binomial))
plot(log(terr_dd_richness1+1), col=inferno(64, direction=1))

df_ml1_fresh<-subset(df_ml1, tolower(df_ml1$freshwater)=="true")
df_ml1_fresh_dd<-subset(df_ml1_fresh, df_ml1_fresh$category=="DD")
length(unique(df_ml1_fresh_dd$binomial))

#
df_ml1_mar<-subset(df_ml1, tolower(df_ml1$marine)=="true")
df_ml1_mar_dd<-subset(df_ml1_mar, df_ml1_mar$category=="DD")
length(unique(df_ml1_mar_dd$binomial))
#


library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(raster)
library(rasterVis)
library(classInt)
library(RColorBrewer)
# Natural Earth shape files -- global (Robinson) projections
# get shapefiles from http://www.naturalearthdata.com



shape_path <- "C:/Users/janbor/Desktop/OneDrive - NTNU/Data/natural_earth_vector/"


coast_shapefile <- paste(shape_path, "50m_physical/ne_50m_coastline.shp", sep="")
ocean_shapefile <- paste(shape_path, "50m_physical/ne_50m_ocean.shp", sep="")
admin0_shapefile <- paste(shape_path, "50m_cultural/ne_50m_admin_0_countries.shp", sep="")
admin1_shapefile <- paste(shape_path, "50m_cultural/ne_50m_admin_1_states_provinces_lakes.shp", sep="")
lakes_shapefile <- paste(shape_path, "50m_physical/ne_50m_lakes.shp", sep="")
bb_shapefile <- paste(shape_path, "50m_physical/ne_50m_graticules_all/ne_50m_wgs84_bounding_box.shp", sep="")
grat30_shapefile <- paste(shape_path, "50m_physical/ne_50m_graticules_all/ne_50m_graticules_30.shp", sep="")


# find out kind of shapefile (lines vs. polygons)
layer <- ogrListLayers(coast_shapefile)
# read the shape file
coast_lines <- readOGR(coast_shapefile, layer=layer)

unproj_proj4string <- proj4string(coast_lines)
unproj_proj4string


# find out kind of shapefile (lines vs. polygons)
layer <- ogrListLayers(ocean_shapefile)
# read the shape file
ocean_poly <- readOGR(ocean_shapefile, layer=layer)

layer <- ogrListLayers(admin0_shapefile)
admin0_poly <- readOGR(admin0_shapefile, layer=layer)

layer <- ogrListLayers(admin1_shapefile)
admin1_poly <- readOGR(admin1_shapefile, layer=layer)

layer <- ogrListLayers(lakes_shapefile)
lakes_poly <- readOGR(lakes_shapefile, layer=layer)

lakes_poly$scalerank <- as.numeric(lakes_poly$scalerank)
lrglakes_poly <- subset(lakes_poly, lakes_poly$scalerank <= 2)

layer <- ogrListLayers(grat30_shapefile)
grat30_lines <- readOGR(grat30_shapefile, layer=layer)

layer <- ogrListLayers(bb_shapefile)
bb_poly <- readOGR(bb_shapefile, layer=layer)
bb_lines <- as(bb_poly, "SpatialLines")


# set Robinson CRS
unproj_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
robin_crs <- CRS("+proj=robin +lon_0=0w")
# define projections
mollCRS <- CRS('+proj=moll')
behrmannCRS <- CRS('+proj=cea +lat_ts=30')


#kaspian see missing
caspian_bb <- as(extent(45, 56, 35, 50), "SpatialPolygons")
proj4string(caspian_bb) <- unproj_proj4string
caspian_poly <- gIntersection(ocean_poly, caspian_bb)
proj4string(caspian_poly) <- unproj_proj4string
caspian_poly_proj <- spTransform(caspian_poly, robin_crs)

# do other projections
bb_poly_proj <- spTransform(bb_poly, robin_crs)
coast_lines_proj <- spTransform(coast_lines, robin_crs)
admin0_poly_proj <- spTransform(admin0_poly, robin_crs)
admin1_poly_proj <- spTransform(admin1_poly, robin_crs)
lakes_poly_proj <- spTransform(lakes_poly, robin_crs)
grat30_lines_proj <- spTransform(grat30_lines, robin_crs)
lrglakes_poly_proj <- spTransform(lrglakes_poly, robin_crs)
ocean_poly_proj <- spTransform(ocean_poly, robin_crs)


# convert polygons to spatial lines
admin0_lines_proj <- as(admin0_poly_proj, "SpatialLines")
bb_lines_proj <- as(bb_poly_proj, "SpatialLines")

# set Robinson CRS
unproj_crs <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
robin_crs <- CRS("+proj=robin +lon_0=0w")
# define projections
mollCRS <- CRS('+proj=moll')
behrmannCRS <- CRS('+proj=cea +lat_ts=30')


sst_longlat<-dd_fraction_fresh
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#sst_rob<-mask(sst_rob, admin0_poly_proj)

globe_rob <- projectRaster(globe, crs=robin_crs)
globe_rob<-mask(globe_rob, admin0_poly_proj)


# plot and save
pngfile <- "C:/Users/janbor/Desktop/plots/fresh.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col="aliceblue", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
plot(sst_rob, col=inferno(64, direction = 1), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=inferno(64, direction=1), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar")),
     axis.args=list(at=seq(0,0.2,0.05),
                    labels=c("0%","5%",
                             "10%","15%","> 20%"), 
                    cex.axis=0.8))
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Percent contribution of data-deficient species to cumulative extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()






sst_longlat<-mar_richness1
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#sst_rob<-mask(sst_rob, admin0_poly_proj)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/plots/mar_ex_risk.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
plot(sst_rob, col=inferno(64, direction = 1), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=inferno(64, direction=1), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar"))
     ,axis.args=list(at=seq(0,100,20),
                     labels=c("0","20",
                              "40","60","80","100"), 
                     cex.axis=0.8)
)
plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Cumulative extinction risk", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()






sst_longlat<-log(fresh_dd_richness1)
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#sst_rob<-mask(sst_rob, admin0_poly_proj)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/plots/fresh_dd_cum.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col="aliceblue", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
plot(sst_rob, col=inferno(64, direction = 1), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=inferno(64, direction=1), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar"))
     ,axis.args=list(at=c(-33,13),
                     labels=c("Low","High"), 
                     cex.axis=0.8)
)
#plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
#plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
#plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Cumulative extinction risk of data-deficient species", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()














sst_longlat<-mar_all1
sst_rob <- projectRaster(sst_longlat, crs=robin_crs)
#sst_rob<-mask(sst_rob, admin0_poly_proj)

# plot and save
pngfile <- "C:/Users/janbor/Desktop/plots/mar_all.png"
png(pngfile, width=16, height=11, units = "cm", res=900)

opar<-par()
par(mfrow=c(1,1))
par(mar=c(3,0,0,0))


plot(bb_poly_proj, col="black", bor="black", lwd=0.5)
plot(grat30_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
plot(sst_rob, col=inferno(64, direction = 1), legend=F, add=TRUE, axes=F)
plot(sst_rob, legend.only=TRUE, col=inferno(64, direction=1), legend.width=1, legend.shrink=0.75,
     horizontal=TRUE, smallplot=c(0.1,0.9, 0.13,0.15), par(mar = par("mar"))
     #,axis.args=list(at=seq(1,13,1),
     #               labels=exp10(c(1:13)), 
     #              cex.axis=0.8)
)
plot(grat30_lines_proj, col="lightgrey", lwd=0.5, add=TRUE)
plot(globe_rob, col="black", legend=F, add=TRUE, axes=F)
plot(admin0_lines_proj, col="black", lwd=0.5, add=TRUE)
#plot(lrglakes_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
#plot(caspian_poly_proj, col="aliceblue", bor="black", lwd=0.5, add=TRUE)
plot(coast_lines_proj, col="black", lwd=0.5, add=TRUE)
plot(bb_lines_proj, col="black", lwd=1.0, add=TRUE)

mtext("Number of species", side=1, adj = 0.1, padj = -3, cex=0.8)
par(mfrow=c(1,1))

par(opar)
dev.off()













