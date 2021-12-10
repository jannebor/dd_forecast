library(sf)
library(fasterize)
library(raster)

#UNEP-WCMC & IUCN 2021 Protected Planet: The World Database on Protected Areas (WDPA) (www.protectedplanet.net)
files<-list.files("/WDPA_Mar2021_Public_shp", pattern = "polygons.shp$",
           recursive = T, full.names = T)

# generate reference raster in approx. 300m resolution
c1 <- raster()
res(c1) <- 0.002777778
crs(c1) <- CRS("+proj=longlat +datum=WGS84")

r <- c1
r[is.na(r)]<-0

for(i in 1:length(files)){
  WDPA<-st_read(files[i])
  WDPA_ras<-fasterize(WDPA, c1)
  WDPA_ras[is.na(WDPA_ras)]<-0
  r <- r + WDPA_ras
}

r[r>1] <- 1

# aggregate by averaging
protected_areas_fraction_res1 <- aggregate(x = r, fact=0.5/res(r)[1], fun=mean)

# snap to reference raster
protected_areas_fraction_res1<-resample(protected_areas_fraction_res1, c1, method="bilinear")
protected_areas_fraction_res1<-extend(protected_areas_fraction_res1, extent(c1))
protected_areas_fraction_res1<-crop(protected_areas_fraction_res1, c1)

writeRaster(protected_areas_fraction_res1, filename = "protected_areas_fraction_res1.tif", overwrite = T)
