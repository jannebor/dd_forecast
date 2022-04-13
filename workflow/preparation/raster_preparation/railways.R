library(raster)

#railways from https://doi.org/10.1038/sdata.2016.67
rails <- raster("Railways.tif")

# generate reference raster
c1 <- raster()
res(c1) <- 0.5
crs(c1) <- CRS("+proj=longlat +datum=WGS84")

rails <- projectRaster(rails, crs=CRS("+proj=longlat +datum=WGS84"))

rails[!is.na(rails)]<-1
rails[is.na(rails)]<-0

# aggregate by averaging
rails <- aggregate(x = rails, fact=res(c1)[1]/res(rails)[1], fun=mean)
  
# snap to reference raster
rails<-resample(rails, c1, method="bilinear")
rails<-extend(rails, extent(c1))
rails<-crop(rails, c1)
  
writeRaster(rails, filename = "Railways_WGS84_binary_res2.tif", overwrite = T)


