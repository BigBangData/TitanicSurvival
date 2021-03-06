#!/usr/bin/env python
# coding: utf-8
# author: Marcelo Sanches 
# date: 01/18/2020
# purpose: modeling functions to help visualize intermediate results during the modeling effort
# version: 2
# version updates: Add transform and plot functions for XGBoost models

# import modules
import matplotlib.pyplot as plt
import numpy as np 
import pandas as pd 


def transform_gridsearch_results(grid_search):
    """Given a grid search object of random forest models, perform various transforms
    to return dataframe subsets for each category of interest.
    """

    # extracting detailed information per CV fold
    cv_res0 = grid_search.cv_results_['split0_test_score']
    cv_res1 = grid_search.cv_results_['split1_test_score']
    cv_res2 = grid_search.cv_results_['split2_test_score']
    cv_res3 = grid_search.cv_results_['split3_test_score']
    cv_res4 = grid_search.cv_results_['split4_test_score']
    
    
    # constructing a main dataframe with the data for plotting the results
    param_array = grid_search.cv_results_['params']
    
    bootstraps, criteria, estimators = [], [], []
    
    for i in param_array:
        bootstraps.append(i['bootstrap'])
        criteria.append(i['criterion'])
        estimators.append(i['n_estimators'])
    
    data = pd.DataFrame(list(zip(bootstraps, criteria, estimators, 
                                 cv_res0, cv_res1, cv_res2, cv_res3, cv_res4))
                        ,columns = ['bootstrap','criterion','n_estimators',
                                    'cv0','cv1','cv2','cv3','cv4'])
    
    # creating subsets for each of the four categories
    boot_gini = data[(data.bootstrap == True)  & (data.criterion == 'gini')].copy()
    boot_entr = data[(data.bootstrap == True)  & (data.criterion == 'entropy')].copy()
    noot_gini = data[(data.bootstrap == False) & (data.criterion == 'gini')].copy()
    noot_entr = data[(data.bootstrap == False) & (data.criterion == 'entropy')].copy()
    
    return(boot_gini, boot_entr, noot_gini, noot_entr)


def plot_single(df, pos, title):
    """Given a dataframe, a position in a 2x2 plot grid, and a title,
    construct a line plot for each cv fold.
    """

    x = list(df['n_estimators'])
    
    Ys = list()
    for i in ['cv0','cv1','cv2','cv3','cv4']:
        Ys.append(df[i])
    
    plt.subplot(2, 2, pos)
    plt.title(title)
    plt.axis([0, 5000, .75, .85]) 
    
    for i in range(len(Ys)):
        plt.plot(x, list(Ys[i]))        
        
        
def plot_means(df, grid_search):
    """Given a df for labels and the original grid search object, 
    construct a line plot for the mean accuracy per number of estimators 
    for each dataframe.
    """

    x = list(df['n_estimators'])

    plt.rcParams['figure.figsize'] = [12, 6]
    
    plt.axis([0, 5000, .79, .83]) 
    
    Y0 = grid_search.cv_results_['mean_test_score'][:20]
    Y1 = grid_search.cv_results_['mean_test_score'][20:40]
    Y2 = grid_search.cv_results_['mean_test_score'][40:60]
    Y3 = grid_search.cv_results_['mean_test_score'][60:80]
    
    plt.plot(x, Y0, label='Bootstrapped - Gini')
    plt.plot(x, Y1, label='Bootstrapped - Entropy')
    plt.plot(x, Y2, label='Not Bootstrapped - Gini')
    plt.plot(x, Y3, label='Not Bootstrapped - Entropy')
    
    plt.title('Mean Accuracies')
    plt.legend(loc='upper left')
    
    
def elbow_plot(labs, imps):
    """Given labels and a vector of importances, 
    plot an elbow plot of importances."
    """

    plt.plot(labs, list(imps))
    plt.title('Feature Importance')
    plt.xticks(labs, rotation='vertical')
    
def transform_gridsearch_results2(grid_search):
    """Given a grid search object of xgboost models, perform various transforms
    to return dataframe subsets for each learning rate.
    """

    # extracting detailed information per CV fold
    cv_res0 = grid_search.cv_results_['split0_test_score']
    cv_res1 = grid_search.cv_results_['split1_test_score']
    cv_res2 = grid_search.cv_results_['split2_test_score']
    cv_res3 = grid_search.cv_results_['split3_test_score']
    cv_res4 = grid_search.cv_results_['split4_test_score']
    
    
    # constructing a main dataframe with the data for plotting the results
    param_array = grid_search.cv_results_['params']
    
    learning_rate, n_estimators = [], []
    
    for i in param_array:
        learning_rate.append(i['learning_rate'])
        n_estimators.append(i['n_estimators'])
    
    data = pd.DataFrame(list(zip(learning_rate, n_estimators, 
                                 cv_res0, cv_res1, cv_res2, cv_res3, cv_res4))
                        ,columns = ['learning_rate', 'n_estimators',
                                    'cv0', 'cv1', 'cv2', 'cv3', 'cv4'])
    
    # creating subsets for each learning rate
    l_0 = data[(data.learning_rate == 0.10)].copy()
    l_1 = data[(data.learning_rate == 0.11)].copy()
    l_2 = data[(data.learning_rate == 0.12)].copy()
    l_3 = data[(data.learning_rate == 0.13)].copy()
    
    l_4 = data[(data.learning_rate == 0.14)].copy()
    l_5 = data[(data.learning_rate == 0.15)].copy()
    l_6 = data[(data.learning_rate == 0.16)].copy()
    l_7 = data[(data.learning_rate == 0.17)].copy()
    
    l_8 = data[(data.learning_rate == 0.18)].copy()
    l_9 = data[(data.learning_rate == 0.19)].copy()
    l_10 = data[(data.learning_rate == 0.20)].copy()
    l_11 = data[(data.learning_rate == 0.21)].copy()

    return(l_0, l_1, l_2, l_3, l_4, l_5, l_6, l_7, l_8, l_9, l_10, l_11) 
    
def plot_single2(df, pos, title):
    """Given a dataframe, a position in a 2x2 plot grid, and a title,
    construct a line plot for each cv fold.
    """

    x = list(df['n_estimators'])
    
    Ys = list()
    for i in ['cv0','cv1','cv2','cv3','cv4']:
        Ys.append(df[i])
    
    plt.rcParams['figure.figsize'] = [15, 9]
    plt.subplot(2, 2, pos)
    plt.title(title)
    plt.axis([100, 200, .78, .88]) 
    
    for i in range(len(Ys)):
        plt.plot(x, list(Ys[i]))        