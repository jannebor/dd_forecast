library(h2o)
library(stringr)
load(file="~/GitHub/dd_forecast/dataframes/df_ml1")

h2o.init(nthreads=4, max_mem_size="30g")

model_ids<-list.files("~/GitHub/dd_forecast/classifier/Partition1/all_models", pattern = ".zip$", full.names = T)

# retrieve variable importance for each trained model (except ensembles)
varimp_all<-data.frame()
for(i in 1:length(model_ids)){
  mm<-h2o.import_mojo(model_ids[i])
  
  if(i %in% grep("Ensemble", model_ids)){
    next
  } else {
    varimp<-as.data.frame(mm@model$variable_importances)
    varimp$model<-paste(str_remove(basename(model_ids[i]),pattern=".zip"))
    varimp_all<-rbind(varimp_all, varimp)
    
  }
}

#save(varimp_all, file="~/GitHub/dd_forecast/dataframes/varimp_all")
load(file="~/GitHub/dd_forecast/dataframes/varimp_all5")

# merge multiple features for each variable, i.e. min, max, mean, etc.
varimp_all$variable<-str_remove(varimp_all$variable, pattern="occ_")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="min.")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="max.")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="mean.")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="median.")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="_min")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="_max")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="_mean")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="_median")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="_300aggr60_10min_extr_30min")
varimp_all$variable<-str_remove(varimp_all$variable, pattern="_300min_aggr60_10min_extr_30min")
varimp_all$variable<-str_remove(varimp_all$variable, pattern=".tif")

# merge models into model families, i.e. DNN, DRF, GBM, GLM, XRT
varimp_all$model<-str_remove(varimp_all$model, pattern="_AutoML_20210803_100543")
varimp_all$model_family<-str_remove(str_sub(varimp_all$model, 1, 13), pattern="_")
varimp_all$model_family<-str_remove(varimp_all$model_family, "1")
varimp_all$model_family<-str_remove(varimp_all$model_family, "2")
varimp_all$model_family<-str_remove(varimp_all$model_family, "3")
varimp_all$model_family<-str_remove(varimp_all$model_family, "4")
varimp_all$model_family<-str_remove(varimp_all$model_family, "5")
varimp_all$model_family[which(varimp_all$model_family=="GBMgrid___m")]<-"GBM"
varimp_all$model_family[which(varimp_all$model_family=="DeepLearning")]<-"DNN"

# calculate the average percentage for each variable across all model families
varimp_all_avg<-aggregate(varimp_all$percentage~varimp_all$model_family+varimp_all$variable, FUN=mean)
names(varimp_all_avg)<-names(varimp_all)[c(6,1,4)]

# select the top50 variables across all model families
varimp_all_sum<-aggregate(varimp_all_avg$percentage~varimp_all_avg$variable, FUN=sum)
names(varimp_all_sum)<-names(varimp_all_avg)[c(2,3)]
varimp_all_sum<-varimp_all_sum[order(varimp_all_sum$percentage, decreasing = T),]
varimp_top50<-varimp_all_sum[1:50,]
varimp_all_avg<-varimp_all_avg[which(varimp_all_avg$variable %in% varimp_top50$variable),]
varimp_all_avg<-varimp_all_avg[order(varimp_all_avg$percentage, decreasing = F),]


# change top50 to meaningful variable names 
varimp_all_avg$variable[which(varimp_all_avg$variable=="class")]<-"Taxonomic class"
varimp_all_avg$variable[which(varimp_all_avg$variable=="systems")]<-"Environmental domains"
varimp_all_avg$variable[which(varimp_all_avg$variable=="n_powerplants")]<-"Number of powerplants"
varimp_all_avg$variable[which(varimp_all_avg$variable=="phylum")]<-"Taxonomic phylum"
varimp_all_avg$variable[which(varimp_all_avg$variable=="n_dams")]<-"Number of dams"
varimp_all_avg$variable[which(varimp_all_avg$variable=="countries")]<-"Number of native countries"
varimp_all_avg$variable[which(varimp_all_avg$variable=="X40_ESACCI_LC")]<-"Mosaic natural vegetation"
varimp_all_avg$variable[which(varimp_all_avg$variable=="X30_ESACCI_LC")]<-"Mosaic cropland"
varimp_all_avg$variable[which(varimp_all_avg$variable=="X130_ESACCI_LC")]<-"Grassland"
varimp_all_avg$variable[which(varimp_all_avg$variable=="X100_ESACCI_LC")]<-"Mosaic tree/shrub"
varimp_all_avg$variable[which(varimp_all_avg$variable=="X50_ESACCI_LC")]<-"Evergreen broadleaved forest"
varimp_all_avg$variable[which(varimp_all_avg$variable=="X120_ESACCI_LC")]<-"Shrubland"
varimp_all_avg$variable[which(varimp_all_avg$variable=="X110_ESACCI_LC")]<-"Mosaic herbaceous cover"
varimp_all_avg$variable[which(varimp_all_avg$variable=="CHELSA_bio10_02")]<-"Mean diurnal air temperature range"
varimp_all_avg$variable[which(varimp_all_avg$variable=="CHELSA_bio10_13")]<-"Precipitation amount of the wettest month"
varimp_all_avg$variable[which(varimp_all_avg$variable=="CHELSA_bio10_15")]<-"Precipitation seasonality"
varimp_all_avg$variable[which(varimp_all_avg$variable=="CHELSA_bio10_16")]<-"Mean monthly precipitation amount of the wettest quarter"
varimp_all_avg$variable[which(varimp_all_avg$variable=="Correlation_01_05_1km_int16")]<-"Linear dependency of EVI on adjacent pixels"
varimp_all_avg$variable[which(varimp_all_avg$variable=="cpi")]<-"Corruption index"
varimp_all_avg$variable[which(varimp_all_avg$variable=="cv_01_05_1km_uint16")]<-"Normalized dispersion of EVI"
varimp_all_avg$variable[which(varimp_all_avg$variable=="evenness_01_05_1km_uint16")]<-"Evenness of EVI"
varimp_all_avg$variable[which(varimp_all_avg$variable=="dem_d_lzw")]<-"Demersal destructive fishing"
varimp_all_avg$variable[which(varimp_all_avg$variable=="forest_cover_loss_res")]<-"Forest cover loss"
varimp_all_avg$variable[which(varimp_all_avg$variable=="hdi")]<-"Human Development Index"
varimp_all_avg$variable[which(varimp_all_avg$variable=="hdi_dev")]<-"Deviation Human Development Index"
varimp_all_avg$variable[which(varimp_all_avg$variable=="HFP1993_int_wgs84_res")]<-"Human Footprint Index 1993"
varimp_all_avg$variable[which(varimp_all_avg$variable=="HFP2009_int_wgs84_res")]<-"Human Footprint Index 2009"
varimp_all_avg$variable[which(varimp_all_avg$variable=="Homogeneity_01_05_1km_uint16")]<-"Similarity of EVI between adjacent pixels"
varimp_all_avg$variable[which(varimp_all_avg$variable=="lulc.human.modification.terrestrial.systems_geographic_res1")]<-"Human modification"
varimp_all_avg$variable[which(varimp_all_avg$variable=="Maximum_01_05_1km_uint16")]<-"Dominance of EVI combinations between adjacent pixels"
varimp_all_avg$variable[which(varimp_all_avg$variable=="obs_cells")]<-"Number of GBIF/OBIS occurrence cells"
varimp_all_avg$variable[which(varimp_all_avg$variable=="panela_invthreat_proj")]<-"Species invasion threat"
varimp_all_avg$variable[which(varimp_all_avg$variable=="panelb_intro_proj")]<-"Species introduction threat"
varimp_all_avg$variable[which(varimp_all_avg$variable=="panelc_dist_proj")]<-"Species establishment threat"
varimp_all_avg$variable[which(varimp_all_avg$variable=="panelg_ag_proj")]<-"Agricultural increase"
varimp_all_avg$variable[which(varimp_all_avg$variable=="pesticides_res")]<-"Pesticide application rate"
varimp_all_avg$variable[which(varimp_all_avg$variable=="Popdensity1990_wgs84_res")]<-"Population density 1990"
varimp_all_avg$variable[which(varimp_all_avg$variable=="Popdensity2010_wgs84_res")]<-"Population density 2010"
varimp_all_avg$variable[which(varimp_all_avg$variable=="proactive")]<-"Proactive national response capacity to IAS"
varimp_all_avg$variable[which(varimp_all_avg$variable=="reactive")]<-"Reactive national response capacity to IAS"
varimp_all_avg$variable[which(varimp_all_avg$variable=="protected_areas_fraction_res1")]<-"Fraction of protected land"
varimp_all_avg$variable[which(varimp_all_avg$variable=="range_01_05_1km_uint16")]<-"Range of EVI"
varimp_all_avg$variable[which(varimp_all_avg$variable=="range_extent")]<-"Extent of occurrence"
varimp_all_avg$variable[which(varimp_all_avg$variable=="road_density_res")]<-"Density of roads"
varimp_all_avg$variable[which(varimp_all_avg$variable=="shannon_01_05_1km_uint16")]<-"Shannon diversity of EVI"
varimp_all_avg$variable[which(varimp_all_avg$variable=="simpson_01_05_1km_uint16")]<-"Simpson diversity of EVI"
varimp_all_avg$variable[which(varimp_all_avg$variable=="std_01_05_1km_uint16")]<-"Dispersion of EVI"
varimp_all_avg$variable[which(varimp_all_avg$variable=="Uniformity_01_05_1km_uint16")]<-"Orderliness of EVI"
varimp_all_avg$variable<-factor(varimp_all_avg$variable, levels = unique(varimp_all_avg$variable))
names(varimp_all_avg)[3]<-"Percentage"

# show heatmap of top50 across variables and model families
library(ggplot2)
ggplot(varimp_all_avg, aes(model_family, variable, fill= Percentage)) + 
  geom_tile() +
  ylab("Variable") +
  xlab("Model family") +
  theme(element_blank()) +
  scale_fill_viridis(discrete=FALSE, direction = -1, option= "D")

