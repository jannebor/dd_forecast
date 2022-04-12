# Importing required packages
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier


train_data = pd.read_csv('C:/Users/janbor/Documents/GitHub/dd_forecast/dataframes/train_df_v2.csv')


#test_data  = pd.read_csv('../input/house-prices-advanced-regression-techniques/test.csv')

X_train       = train_data.drop("category_group", axis = 1)
y_train       = train_data["category_group"]

# Converting categorical variables into dummy variable
X_dummy = pd.get_dummies(X_train)

# Defining Random Forest Classifier
randomForest = RandomForestClassifier(n_jobs = -1, class_weight = 'balanced', max_depth = 5)
randomForest.fit(X_dummy, y_train)

from boruta import BorutaPy

# Defining parameters of boruta object for feature selection
feature_selection = BorutaPy(randomForest, n_estimators = "auto", verbose = 2,
    max_iter=100, random_state = 1, perc = 100,
    alpha=0.05)

import time
start = time.process_time()
# Get relevant features
feature_selection.fit(np.array(X_dummy), np.array(y_train))

print(time.process_time() - start)

feature_selection.support_
feature_selection.support_weak_
feature_selection.ranking_
feature_rankings = list(zip(X_dummy.columns,
                           feature_selection.ranking_,
                           feature_selection.support_))

feature_rankings
feature_selection.support_weak_


import csv 
    
# field names 
fields = ['Name', 'Ranking', 'Support'] 
    
# name of csv file 
filename = "C:/Users/janbor/Documents/GitHub/dd_forecast/dataframes/train_df_v2features_ranking100.csv"
    
# writing to csv file 
with open(filename, 'w') as csvfile: 
    # creating a csv writer object 
    csvwriter = csv.writer(csvfile) 
        
    # writing the fields 
    csvwriter.writerow(fields) 
        
    # writing the data rows 
    csvwriter.writerows(feature_rankings)





#
# Printing the list of varibales which got selected
count = 0
for i in feature_rankings:
    if i[2] == True:
        count += 1
        print(f'count: {count}  Name: {i[0]} Rank: {i[1]}')
        
        
#        






from BorutaShap import BorutaShap


import numpy as np
from boruta import BorutaPy
from sklearn.ensemble import RandomForestRegressor



# If no model is selected default is the Random Forest
# If classification is True it is a classification problem
Feature_Selector = BorutaShap(importance_measure='shap', classification=True)
Feature_Selector.fit(X=X_dummy, y=y_train, n_trials=10, random_state=1)









# no model selected default is Random Forest, if classification is True it is a Classification problem
Feature_Selector = BorutaShap(importance_measure='shap',
                              classification=True)
                              
Feature_Selector.fit(X=X, y=y, n_trials=50, sample=False,
            	     train_or_test = 'test', normalize=True,
		     verbose=True)                              

