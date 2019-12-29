#!/usr/bin/env python
# coding: utf-8
# author: Marcelo Sanches 
# date: 12/29/2019
# purpose: A series of modeling functions to predict survival in the Titanic as per the Kaggle Titanic competition
# version: 1
# version updates: None

# import modules
import numpy as np 
import pandas as pd 
import os
import re 

def predict_SGD(data, labels, classifier):
    """Make predictions using a SGD classifier and return the accuracy.
    """
    
    np.random.seed(473)
    predictions = classifier.predict(data)
    results = pd.DataFrame({'preds': predictions,
                            'labels':labels})

    results['accurate'] = 0
    for i in range(0, len(results)):
        if results['labels'][i:i+1].values == results['preds'][i:i+1].values:
            results['accurate'][i:i+1] = 1
        else:
            pass
    
    accuracy = round(sum(results['accurate'])/results.shape[0],4)
    
    return(accuracy)



