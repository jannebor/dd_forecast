library(raster)
library(stringr)


# required third-party data sources: 
#land cover: http://maps.elie.ucl.ac.be/CCI/viewer/download.php
#Bio oracle: https://bio-oracle.org (https://doi.org/10.1111/geb.12693)
#Karger et al. 2018. Data from: Climatologies at high resolution for the earth’s land surface areas doi:10.5061/dryad.kd1d4
#Venter et al. 2016 Global terrestrial Human Footprint maps for 1993 and 2009 doi:10.1038/sdata.2016.67
#Kennedy et al. 2019 Managing the middle: A shift in conservation priorities based on the global human modification gradient doi:10.1111/gcb.14549
#Seto et al. 2012 Global forecasts of urban expansion to 2030 and direct impacts on biodiversity and carbon pools doi:10.1073/pnas.1211658109
#Hansen et al. 2013 High-Resolution Global Maps of 21st-Century Forest Cover Change doi:10.1126/science.1244693
#Tuanmu & Jetz 2015 A global, remote sensing-based characterization of terrestrial habitat heterogeneity for biodiversity and ecosystem modelling doi:10.1111/geb.12365
#Maggi et al. 2019 PEST-CHEMGRIDS, global gridded maps of the top 20 crop-specific pesticide application rates from 2015 to 2025 doi:10.1038/s41597-019-0169-4
#Domisch et al. 2015 Near-global freshwater-specific environmental variables for biodiversity analyses in 1 km resolution doi:10.1038/sdata.2015.73
#Early et al. 2016 Global threats from invasive alien species in the twenty-first century and national response capacities (doi:10.1038/ncomms12485)
#Halpern et al. 2008 A Global Map of Human Impact on Marine Ecosystems (doi:10.1126/science.1149345; 10.1038/ncomms8615)
#UNEP-WCMC & IUCN 2021 Protected Planet: The World Database on Protected Areas (WDPA) (www.protectedplanet.net)


# NOTE: .nc files need to be converted to .tif before stacking

#list raster files
files<-list.files(path = "...", pattern = ".tif$", all.files = FALSE, recursive = TRUE,
                  full.names = T)
nam<-basename(files)
nam<-str_remove(nam, ".tif")

# create reference raster
c1 <- raster()
res(c1) <- 0.5
crs(c1) <- CRS("+proj=longlat +datum=WGS84")

# create raster stack
predictors_stack<-stack()
for (f in 1:length(files)){
  
  ras<-raster(paste(files[f]))
  ras<-resample(ras, c1)
  ras<-extend(ras, extent(c1))
  ras<-crop(ras, c1)
  
  names(ras)<-nam[f]
  
  predictors_stack <- stack(predictors_stack, ras)

}

writeRaster(predictors_stack,"~/GitHub/dd_forecast/files/raster/predictors_stack.grd", format="raster")


