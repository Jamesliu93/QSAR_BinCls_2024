# -*- coding: utf-8 -*-
"""
Created on Tue Nov  1 08:34:01 2022
Cross validation for classification models
@author: jpeeples, Dillon Richards
"""

import pandas as pd
import numpy as np
from sklearn.neural_network import MLPClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
import time
import scipy.stats as stats
from sklearn.model_selection import RandomizedSearchCV
import os
from warnings import simplefilter
import pickle

# ignore all future warnings
simplefilter(action='ignore', category=FutureWarning)

def report(results, n_top=3):
    for i in range(1, n_top + 1):
        candidates = np.flatnonzero(results["rank_test_score"] == i)
        for candidate in candidates:
            print("Model with rank: {0}".format(i))
            print(
                "Mean validation score: {0:.3f} (std: {1:.3f})".format(
                    results["mean_test_score"][candidate],
                    results["std_test_score"][candidate],
                )
            )
            print("Parameters: {0}".format(results["params"][candidate]))
            print("")
       
            
def cross_validation(X,y,Params,saved_directory):
    
    #Initialize models
    names = [
          "KNN",
        "Random Forest",
        "MLP",
    ]
    
    classifiers = [
        KNeighborsClassifier(),
        RandomForestClassifier(),
        MLPClassifier(max_iter=1000),
    ]
    
    #Peform cross validation for each models "important" hyperparameters
    param_dist = [
            {"n_neighbors": np.random.random_integers(low=1,high=100,size=20), "weights": ['uniform','distance']}, 
            {"max_depth":np.random.random_integers(low=1,high=50,size=20), "n_estimators":np.random.random_integers(low=10,high=200,size=20)}, #RandomForest
            {"alpha": stats.uniform(0.0001,1), "hidden_layer_sizes":np.random.random_integers(low=20,high=200,size=20)}  #MLP      
            ]
    

    #Perform cross validation on all models and return best results
    best_classifiers = {}
    
    print('Starting Cross Validation...')
    cv_start = time.time()
    for x in range(len(classifiers)):
        
        #If 7000 bus case and using pretrained model, do not perform cross validation
        if (Params['use_pretrained'] and (Params['Dataset'] == '7000 Bus')):
            print('Using pretrained {}'.format(names[x]))
            
            #Load pretrained_model
            saved_model_location = './Pretrained_Models/{}.sav'.format(names[x])
            
            #Save best classifiers in dictionary
            best_classifiers[names[x]] = pickle.load(open(saved_model_location,'rb'))
        
        else:
            random_search = RandomizedSearchCV(
                classifiers[x], param_distributions=param_dist[x], n_iter=Params['n_iter_search'])
            
            start = time.time()
            random_search.fit(X, y)
            print(
                "RandomizedSearchCV took %.2f seconds for %d candidates parameter settings for %s."
                % ((time.time() - start), Params['n_iter_search'],names[x])
            )
            
            #Print report if desired
            if not(Params['verbose'] == 0):
                report(random_search.cv_results_)
            
            #Save report in pandas dataframe in desired
            if Params['save_results']:
                
                #Generate directory for classier
                classifier_dir = '{}{}'.format(saved_directory,names[x])
                if not os.path.exists(classifier_dir):
                    os.makedirs(classifier_dir)
                 
                #Save results in folder
                df_report = pd.DataFrame.from_dict(random_search.cv_results_)
                df_report.to_csv('{}/{}_Cross_Validation.csv'.format(classifier_dir,
                                                                     names[x]))
            
            #Save best classifiers in dictionary
            best_classifiers[names[x]] = random_search.best_estimator_
     
    time_elapsed = time.time() - cv_start
    
    print('Cross validation completed in {:.0f}m {:.0f}s'.format(time_elapsed // 60, time_elapsed % 60))
    return best_classifiers
    
    






