# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 12:00:26 2023
Generate precision recall curves for each classifier
@author: jpeeples
"""

import matplotlib.pyplot as plt
import pandas as pd
from itertools import cycle
import numpy as np
from sklearn.metrics import (average_precision_score, precision_recall_curve,
                             precision_recall_fscore_support, PrecisionRecallDisplay)

def generate_PR_Curves(X,y,classifier,classifier_name,classifier_directory,
                       data_directory, plot_fuel_types=False):
    
    
    #Set title 
    plot_title = 'Preicison-Recall Curves for {}'.format(classifier_name)
    
    #Y_test and y_score will be N x G (N is the number of time points/samples and G is for the number of generators)
    #For each class
    precision = dict()
    recall = dict()
    average_precision = dict()
    
    #Get probablitity scores from classfier
    y_score = classifier.predict_proba(X)
    
    #Convert y_score to matrix shape matching true predictions
    #(for PR curve, only need score for postive class (e.g., on status))
    y_prob = []
    
    #If y_score is a list, need to reshape into N x G proabiliies
    if type(y_score) == list:
        for output in range(0,len(y_score)):
            y_prob_temp = y_score[output]
            
            if y_prob_temp.shape[-1] > 1:
                y_prob.append(np.expand_dims(y_prob_temp[:,-1], axis=1))
            else:
                y_prob.append(y_prob_temp)
        
        y_prob = np.concatenate(y_prob,axis=1)
    else:
        y_prob = y_score
    
    #Generate figure for PR curves
    _, ax = plt.subplots(figsize=(7, 8))
    
    if plot_fuel_types:
        #Read labels from CSV (only works for 7000 bus case with same parameter 
        # as paper/default settings)
        generator_data = pd.read_csv('{}{}.csv'.format(data_directory,'7000 Bus Generator Labels'))
        n_classes=len(generator_data["FuelType"].unique())
        fuel_types = generator_data["FuelType"].unique()
        fuel_types.sort()
        fuel_list = generator_data["FuelType"]
        
        #Get predictions
        y_pred = classifier.predict(X)
        
        #Initialize array for metric scores for each generator (macro and micro averages for each generator)
        metric_names = ['Precision (Macro)', 'Precision (Micro)', 
                        'Recall (Macro)', 'Recall (Micro)',
                        'F1 Score (Macro)', 'F1 Score (Micro)']
        metric_table = np.zeros((n_classes,6))
        for i in range(n_classes):
            #Use the fuel types to select the correct samples for evaluation
            y_test_subset =   y[:,fuel_list==fuel_types[i]]
            y_prob_subset = y_prob[:,fuel_list==fuel_types[i]]
            y_pred_subset = y_pred[:,fuel_list==fuel_types[i]]
            precision[i], recall[i], _ = precision_recall_curve( np.ravel(y_test_subset), np.ravel( y_prob_subset))
            average_precision[i] = average_precision_score(np.ravel(y_test_subset), np.ravel( y_prob_subset))
            
            #Compute macro and micro averages
            macro_scores = precision_recall_fscore_support(y_test_subset, y_pred_subset,average='macro')[:-1]
            micro_scores = precision_recall_fscore_support(y_test_subset, y_pred_subset,average='micro')[:-1]
            
            #Format scores for tables (macro and micro scores organized for each metric)
            metric_table[i,:] = np.ravel([macro_scores,micro_scores],'F')
    
        #Convert table to Pandas array
        df = pd.DataFrame(np.round(metric_table,decimals=3),columns=metric_names,index=fuel_types)
        
        #Save to excel file
        df.to_csv('{}/{}.csv'.format(classifier_directory,'Fuel_Types_Metrics'))
        
        colors = cycle(["navy", "turquoise", "darkorange", "cornflowerblue", "teal", "slategray", "brown"])
        
        for i, color, gen_type in zip(range(n_classes), colors, generator_data["FuelType"].unique()):
            display = PrecisionRecallDisplay(
                recall=recall[i],
                precision=precision[i],
                average_precision=average_precision[i],
            )
            display.plot(ax=ax, name=f"Precision-Recall for {gen_type}", color=color)
    
    # A "micro-average": quantifying score on all classes jointly
    precision["micro"], recall["micro"], _ = precision_recall_curve(
        np.ravel( y), np.ravel(y_prob)
    )
    average_precision["micro"] = average_precision_score(np.ravel(y), np.ravel(y_prob), average="micro")
    
    # A "macro-average": quantifying score on all classes jointly
    precision["macro"], recall["macro"], _ = precision_recall_curve(
       np.ravel( y), np.ravel(y_prob)
    )
    average_precision["macro"] = average_precision_score(np.ravel( y), np.ravel(y_prob), average="macro")
    
    
    f_scores = np.linspace(0.2, 0.8, num=4)
    labels = []
    for f_score in f_scores:
        x = np.linspace(0.01, 1)
        y = f_score * x / (2 * x - f_score)
        (l,) = plt.plot(x[y >= 0], y[y >= 0], color="gray", alpha=0.2)
        plt.annotate("f1={0:0.1f}".format(f_score), xy=(0.9, y[45] + 0.02))
    
    display = PrecisionRecallDisplay(
        recall=recall["micro"],
        precision=precision["micro"],
        average_precision=average_precision["micro"],
    )
    display.plot(ax=ax, name="Micro-average Precision-Recall", color="gold")
    
    display2 = PrecisionRecallDisplay(
        recall=recall["macro"],
        precision=precision["macro"],
        average_precision=average_precision["macro"],
    )
    display2.plot(ax=ax, name="Macro-average Precision-Recall", color="orange")
 
    # add the legend for the iso-f1 curves
    handles, labels = display.ax_.get_legend_handles_labels()
    handles.extend([l])
    labels.extend(["iso-f1 curves"])
    # set the legend and the axes
    ax.set_xlim([0.0, 1.0])
    ax.set_ylim([0.0, 1.01])
    plt.rcParams.update({'font.size': 14})
    ax.legend(handles=handles, labels=labels, loc="center left",fontsize=11)
    
    plt.rcParams.update({'font.size': 15})
    plt.show()
    plt.savefig('{}/{}.png'.format(classifier_directory,plot_title))
    plt.close('all')

        