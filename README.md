## Extinction Risk of Data Deficient Species

Numerous species of the IUCN Red List of Threatened Species are classified as [Data Deficient](https://www.iucnredlist.org/search?permalink=2ed13c01-7e0e-4596-a100-38ed47d30a99). This code was used to predict probabilities of being threatened by extinction for Data Deficient species containing range map data available from the IUCN [spatial data download](https://www.iucnredlist.org/resources/spatial-data-download). The classifier can be applied for individual species using our [web application](https://ml-extinctionrisk.indecol.no/) (alpha version).

![Number of threatened DD species](figs/ext_data_fig4_trp.png)

## Predictor data

#### Note: The following datasets need to be downloaded individually from third-party sources for reproducing the study, otherwise skip to Model preparation:

-   Water scarcity footprints ([Boulay et al. 2018](https://doi.org/10.1007/s11367-017-1333-8))
-   Freshwater connectivity indices ([Barbarossa et al. 2020](https://doi.org/10.1073/pnas.1912776117))
-   Global Database of Power Plants ([Byers et al. 2019](https://datasets.wri.org/dataset/globalpowerplantdatabase))
-   Global dataset of more than 38,000 georeferenced dams ([Mulligan et al. 2020](https://doi.org/10.1038/s41597-020-0362-5))
-   [Human development index](http://hdr.undp.org/sites/default/files/2020_statistical_annex_all.xlsx)
-   [Corruption Perceptions Index 2020](https://images.transparencycdn.org/images/CPI_FULL_DATA_2021-01-27-162209.zip)
-   Global threats from invasive alien species ([Early et al. 2016](https://doi.org/10.1038/ncomms12485))
-   ESA CCI [Land cover](http://maps.elie.ucl.ac.be/CCI/viewer/download.php)
-   Marine data layers for ecological modelling: [Bio-ORACLE](https://bio-oracle.org)
-   Climatologies at high resolution ([Karger et al. 2018](https://doi.org/10.5061/dryad.kd1d4))
-   Global terrestrial Human Footprint maps for 2009 ([Venter et al. 2016](https://doi.org/10.1038/sdata.2016.67))
-   Human modification gradient ([Kennedy et al. 2019](https://doi.org/10.1111/gcb.14549))
-   Urban expansion probabilities ([Seto et al. 2012](https://doi.org/10.1073/pnas.1211658109))
-   Forest Cover Change ([Hansen et al. 2013](https://doi.org/10.1126/science.1244693))
-   Habitat heterogeneity ([Tuanmu & Jetz 2015](https://doi.org/10.1111/geb.12365))
-   Pesticide application rates ([Maggi et al. 2019](https://doi.org/10.1038/s41597-019-0169-4))
-   Freshwater environmental variables ([Domisch et al. 2015](https://doi.org/10.1038/sdata.2015.73))
-   Human Impacts on Marine Ecosystems ([Halpern et al. 2008](https://doi.org/10.1126/science.1149345))
-   World Database on Protected Areas ([UNEP-WCMC & IUCN 2021](www.protectedplanet.net))

Scripts for data pre-processing, e.g., calculating land-use fractions, etc., and stacking all spatial layers are stored in [workflow/preparation/raster_preparation](https://github.com/jannebor/dd_forecast/tree/main/workflow/preparation/raster_preparation) and need to be adjusted individually.

The underlying function for retrieving predictor data from tables, web sources (i.e., IUCN, GBIF & OBIS), and the above downloaded spatial datasets for single species is [workflow/preparation/data_extraction.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/preparation/data_extraction.R). We applied this function for entire spatial datasets in [workflow/preparation/data_extraction_batch.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/preparation/data_extraction_batch.R). The resulting full dataframe (df_ml_v2) is stored as R object in [dataframes/full_data](https://github.com/jannebor/dd_forecast/tree/main/dataframes/full_data).

## Model preparation

#### Full reproducibility (based on code only) is given from this point onwards:

Training (75%) and testing (25%) data was prepared ([workflow/preparation/model_prep.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/preparation/model_prep.R)) for each partition (partition 1: all species, partition 2: marine & non-marine species separately) and stored as R objects in [dataframes/Partition 1](https://github.com/jannebor/dd_forecast/tree/main/dataframes/Partition1) and [dataframes/Partition 2](https://github.com/jannebor/dd_forecast/tree/main/dataframes/Partition2). For each of the partition-specific dataframes features were selected ([workflow/preparation/feature_selection.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/preparation/feature_selection.R)) using the Boruta algorithm ([Kursa & Rudnicki 2010](https://doi.org/10.18637/jss.v036.i11)). Only relevant features were considered during model building.

## Model building

In total 510 models were fitted using [AutoML](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html) in H2O. 222 models were fitted using all species ([workflow/training/model_partition 1.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/training/model_partition1.R)), 134 using only marine species and 154 using only non-marine species ([workflow/training/model_partition 2.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/training/model_partition2.R)). All models were calibrated using 10-fold cross-validation, and ranked in terms of AUC based on the set aside testing data (25%), e.g. for partition 1:

| model_id                                 | auc   | logloss | aucpr | mean_per_class_error | rmse  | mse   |
|------------------------------------------|-------|---------|-------|----------------------|-------|-------|
| StackedEnsemble_AllModels_3\_AutoML_1    | 0.912 | 0.314   | 0.795 | 0.174                | 0.311 | 0.097 |
| StackedEnsemble_AllModels_6\_AutoML_1    | 0.912 | 0.315   | 0.795 | 0.175                | 0.311 | 0.097 |
| StackedEnsemble_AllModels_4\_AutoML_1    | 0.912 | 0.315   | 0.795 | 0.175                | 0.311 | 0.097 |
| StackedEnsemble_AllModels_5\_AutoML_1    | 0.910 | 0.318   | 0.791 | 0.176                | 0.313 | 0.098 |
| StackedEnsemble_BestOfFamily_4\_AutoML_1 | 0.909 | 0.318   | 0.793 | 0.184                | 0.313 | 0.098 |

## Model evaluation

Performance metrics were calculated based on the testing data ([workflow/evaluation/model_performance.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/evaluation/model_performance.R)) and based on reclassified Data Deficient species ([workflow/evaluation/dd_performance.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/evaluation/dd_performance.R)). Permutation variable importance was calculated by measuring performance loss before and after a feature was permuted ([workflow/evaluation/variable_importance.R](https://github.com/jannebor/dd_forecast/blob/main/workflow/evaluation/variable_importance.R)).

![Permutation variable importance](figs/ext_data_fig8_trp.png)

## Predictions

The generated predictions for Data Deficient species are stored in [dd_predictions.csv](https://github.com/jannebor/dd_forecast/blob/main/dataframes/Partition1/predictions/dd_predictions.csv) and show the probability of being threatened by extinction for each species:

| Species                   | Last Assessed | Taxonomic class | Red List Category | Probability of being threatened |
|---------------------------|---------------|-----------------|-------------------|---------------------------------|
| Chirostoma grandocule     | 2018          | Actinopterygii  | Data Deficient    | 95.8%                           |
| Sarcohyla miahuatlanensis | 2019          | Amphibia        | Data Deficient    | 95.8%                           |
| Crossodactylus dantei     | 2008          | Amphibia        | Data Deficient    | 95.4%                           |
| Nyctibatrachus sholai     | 2008          | Amphibia        | Data Deficient    | 95.2%                           |
| Colostethus alacris       | 2016          | Amphibia        | Data Deficient    | 95.2%                           |
| …                         |               |                 |                   |                                 |
