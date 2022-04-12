library(raster)

#Maggi et al. 2019 PEST-CHEMGRIDS, global gridded maps of the top 20 crop-specific pesticide application rates from 2015 to 2025 doi:10.1038/s41597-019-0169-4
r <- list.files("/ferman-v1-pest-chemgrids_geotiff/ApplicationRate/GEOTIFF", pattern = "2015_H.tif", full.names = T)

pesticides<-raster()
res(pesticides) <- 0.5
crs(pesticides) <- CRS("+proj=longlat +datum=WGS84")
pesticides[is.na(pesticides)]<-0

for(i in 1:length(r)){

  ras<-raster(r[i])
  ras<-resample(ras, pesticides, method="bilinear")
  ras<-extend(ras, extent(pesticides))
  ras<-crop(ras, pesticides)
  
  pesticides<-pesticides+ras
  
}

writeRaster(pesticides, filename = "pesticides_res.tif", overwrite = T)


