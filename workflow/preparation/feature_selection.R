library(Boruta)

#apply variable selection on partition specific training data

# Partition 1
# all species:
load(file="~/GitHub/dd_forecast/dataframes/Partition1/train_df_v2")
Boruta(category_group~., data=train_df, maxRuns=50, doTrace=2) -> VarSel
#save(VarSel, file="~/GitHub/dd_forecast/dataframes/Partition1/variable_selection/VarSel")


# Partition 2
#non-marine species:
load(file="~/GitHub/dd_forecast/dataframes/Partition2/train_terr_v2")
Boruta(category_group~., data=train_terr, maxRuns=50, doTrace=2) -> VarSel
#save(VarSel, file="~/GitHub/dd_forecast/dataframes/Partition2/variable_selection/VarSel_terr")

#marine species:
load(file="~/GitHub/dd_forecast/dataframes/Partition2/train_mar_v2")
Boruta(category_group~., data=train_mar, maxRuns=50, doTrace=2) -> VarSel
#save(VarSel, file="~/GitHub/dd_forecast/dataframes/Partition2/variable_selection/VarSel_mar")

