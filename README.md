## Predicting the extinction risk of data-deficient species

Numerous species of the IUCN Red List of Threatened Species are classified as data-deficient. The provided code was used to predict threat intensities across data-deficient species of all spatial data available from [IUCN](https://www.iucnredlist.org/resources/spatial-data-download).

Code for reproducing the results can be found in the folder workflow.

The generated predictions are stored in [dd_predictions.csv](https://github.com/jannebor/dd_forecast/blob/main/dd_predictions.csv) and show the probability of being threatened by extinction for each species:



| Species                         | Taxonomic class | RL category    | Probability of threat |
|---------------------------------|-----------------|----------------|-----------------------|
| Nyctibatrachus sholai           | Amphibia        | data-deficient | 95.4%                 |
| Eleutherodactylus verruculatus  | Amphibia        | data-deficient | 95.3%                 |
| Micrixalus thampii              | Amphibia        | data-deficient | 95.0%                 |
| Crossodactylus dantei           | Amphibia        | data-deficient | 94.7%                 |
| Physalaemus barrioi             | Amphibia        | data-deficient | 94.7%                 |
| Crotaphatrema tchabalmbaboensis | Amphibia        | data-deficient | 94.5%                 |
| Cryptotis lacertosus            | Mammalia        | data-deficient | 94.4%                 |
| Physalaemus erythros            | Amphibia        | data-deficient | 94.4%                 |
| Gastrotheca williamsoni         | Amphibia        | data-deficient | 94.4%                 |
| …                               |                 |                |                       |





