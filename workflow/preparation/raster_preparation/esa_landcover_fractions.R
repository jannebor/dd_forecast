library(raster)

#land cover: http://maps.elie.ucl.ac.be/CCI/viewer/download.php
esa <- raster("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")

# generate reference raster
c1 <- raster()
res(c1) <- 0.5
crs(c1) <- CRS("+proj=longlat +datum=WGS84")

# considered landcover types
lc_types <- c(30, 40, 50, 60, 70, 80, 90,
  100, 110, 120, 130, 140, 150, 160,
  170, 180, 200)

for(i in lc_types){
  
  e<-esa
  # make binary raster of selected landcover type
  e[e >= i & e < i+10]<-1
  e[e!=1]<-0
  
  # aggregate by averaging
  e <- aggregate(x = e, fact=res(c1)[1]/res(e)[1], fun=mean)
  
  # snap to reference raster
  e<-resample(e, c1, method="bilinear")
  e<-extend(e, extent(c1))
  e<-crop(e, c1)
  
  writeRaster(e, filename = paste("X",i,"_ESACCI_LC_300min_aggr60_10min_extr_30min.tif",sep=""), overwrite = T)
  
}


