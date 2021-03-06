#!/usr/bin/env python
# coding: utf-8
# author: Marcelo Sanches 
# date: 12/29/2019
# purpose: A pre-processing pipeline for the Kaggle Titanic competition training data
# version: 1
# version updates: None

# import modules
import numpy as np 
import pandas as pd 
import os
import re 


def extract_title(name):    
    """Given a name from the Name column, extract the title. 
    If it is common return it, else return 'Rare'.
    """
    
    try:
        title = re.search(r',\s(.+?)\.', name).groups()[0]
    except:
        title = ''
    
    if title in ["Mr", "Mrs", "Miss"]:
        return(title)
    else:
        return('Rare')
    
        
def map_fare_perperson(ticket, group):    
    """Counts people per tickets and divides the fare per number of people in a ticket.
    Uses the group helper table with aggregated results.
    """

    row_names=np.array(list(group.index.values))
    loc=np.where(row_names == ticket)[0][0]
    
    return(group['FarePerPerson'][loc:loc+1][0])


def clean_cabin(x):    
    """Extract the Deck information, first letter only, 
    Limit to six groups, A through F, binning other decks to F.
    """
    
    letter=x[0]
    
    if letter not in 'ABCDEF':
        letter='F'
        
    return(letter)


def minmax_scale(X):
    
    def scale(X, x):
        return((x - min(X)) / (max(X) - min(X)))
        
    return(X.map(lambda x: scale(X, x)))


def std_scale(X):

    def scale(X, x):
        return((x - np.mean(X)) / np.std(X))

    return(X.map(lambda x: scale(X, x)))

    
def process_train(train):    
    """Performs all the necessary pre-processing steps for converting the data 
    into a machine-learning ready numpy array.
    """

    # Feature Engineering 
    # -------------------
    
    # create Title        
    train['Title'] = train['Name'].map(lambda x: extract_title(x))
    
    # Create NameLength
    train['NameLength'] = train['Name'].map(lambda x: len(x))
    
    # Create NumRelatives
    train['NumRelatives'] = train['SibSp'] + train['Parch']
    
    # Create FarePerPerson
    train['count'] = 1
    group = train[['Ticket','Fare','count']].groupby('Ticket').sum()
    group['Fare'] = group['Fare']/group['count']
    group['FarePerPerson'] = group['Fare']/group['count']
    train['FarePerPerson'] = train['Ticket'].map(lambda x: map_fare_perperson(x, group))
    
    # Create Deck     
    train['Deck']=train['Cabin'].map(lambda x: clean_cabin(x), na_action='ignore')
    
    # Embarked: impute two missing with majority class
    train['Embarked']=train['Embarked'].fillna('S')
    
    # Age
    # impute with group medians given gender, passenger class, and title
    train['Age'] = train.groupby(['Sex', 'Pclass','Title'])['Age'].\
                                 transform(lambda x: x.fillna(x.median()))
    
    # Binary Indicators
    # -----------------
    
    # Sex
    train['IsMale'] = pd.get_dummies(train['Sex'])['male']
    
    # Embarked
    train['Embarked_S']=pd.get_dummies(train['Embarked'])['S']
    train['Embarked_Q']=pd.get_dummies(train['Embarked'])['Q']
    
    # Title
    train['Title_Mr']=pd.get_dummies(train['Title'])['Mr']
    train['Title_Mrs']=pd.get_dummies(train['Title'])['Mrs']
    train['Title_Miss']=pd.get_dummies(train['Title'])['Miss']
    
    # Pclass
    train['Pclass_1']=pd.get_dummies(train['Pclass'])[1]
    train['Pclass_2']=pd.get_dummies(train['Pclass'])[2]
    
    # Deck
    train['Deck'].fillna('None') # create a None category for NA values
    train['Deck_A']=pd.get_dummies(train['Deck'])['A']
    train['Deck_B']=pd.get_dummies(train['Deck'])['B']
    train['Deck_C']=pd.get_dummies(train['Deck'])['C']
    train['Deck_D']=pd.get_dummies(train['Deck'])['D']
    train['Deck_E']=pd.get_dummies(train['Deck'])['E']
    train['Deck_F']=pd.get_dummies(train['Deck'])['F']
    
    # drop unwanted, redundant columns
    train.drop(['PassengerId', 'Pclass','Name','Sex','SibSp','Parch','Ticket','Fare',
                'Cabin','count','Embarked','Title','Deck'], axis=1, inplace=True)
       
    # Scaling
    # -------
    
    # scale Age, NameLength, NumRelatives, FarePerPerson
    train['Age_minmax'] = minmax_scale(train['Age'])
    train['Age_std'] = std_scale(train['Age'])
    train['NameLength_minmax'] = minmax_scale(train['NameLength'])
    train['NameLength_std'] = std_scale(train['NameLength'])
    train['NumRelatives_minmax'] = minmax_scale(train['NumRelatives'])
    train['NumRelatives_std'] = std_scale(train['NumRelatives'])
    train['FarePerPerson_minmax'] = minmax_scale(train['FarePerPerson'])
    train['FarePerPerson_std'] = std_scale(train['FarePerPerson'])
    
    
    # drop unscaled and standard scaled features
    train.drop(['Age', 'NameLength','FarePerPerson','NumRelatives','Age_std',
                'NameLength_std','FarePerPerson_std','NumRelatives_std'], axis=1, inplace=True)
       
    # convert to numpy arrays
    train_processed = train.to_numpy()
    
    return(train_processed)

def process_test(test_data):
    """Same as process_train except for one NaN value I had to impute again.
    """

    # Feature Engineering 
    
    test_data['Title'] = test_data['Name'].map(lambda x: extract_title(x))
    test_data['NameLength'] = test_data['Name'].map(lambda x: len(x))
    test_data['NumRelatives'] = test_data['SibSp'] + test_data['Parch']
    
    test_data['count'] = 1
    group = test_data[['Ticket','Fare','count']].groupby('Ticket').sum()
    group['Fare'] = group['Fare']/group['count']
    group['FarePerPerson'] = group['Fare']/group['count']
    test_data['FarePerPerson'] = test_data['Ticket'].map(lambda x: map_fare_perperson(x, group))
    
    test_data['Deck']=test_data['Cabin'].map(lambda x: clean_cabin(x), na_action='ignore')
    test_data['Embarked']=test_data['Embarked'].fillna('S')
    test_data['Age'] = test_data.groupby(['Sex', 'Pclass','Title'])['Age'].\
                                     transform(lambda x: x.fillna(x.median()))
    
    test_data['Age'].fillna(test_data['Age'].mean(), inplace=True) # re-impute value
    
    # Dummies
    
    test_data['IsMale'] = pd.get_dummies(test_data['Sex'])['male']
    test_data['Embarked_S']=pd.get_dummies(test_data['Embarked'])['S']
    test_data['Embarked_Q']=pd.get_dummies(test_data['Embarked'])['Q']
    test_data['Title_Mr']=pd.get_dummies(test_data['Title'])['Mr']
    test_data['Title_Mrs']=pd.get_dummies(test_data['Title'])['Mrs']
    test_data['Title_Miss']=pd.get_dummies(test_data['Title'])['Miss']
    test_data['Pclass_1']=pd.get_dummies(test_data['Pclass'])[1]
    test_data['Pclass_2']=pd.get_dummies(test_data['Pclass'])[2]
    test_data['Deck'].fillna('other') # create a None category for NA values
    test_data['Deck_A']=pd.get_dummies(test_data['Deck'])['A']
    test_data['Deck_B']=pd.get_dummies(test_data['Deck'])['B']
    test_data['Deck_C']=pd.get_dummies(test_data['Deck'])['C']
    test_data['Deck_D']=pd.get_dummies(test_data['Deck'])['D']
    test_data['Deck_E']=pd.get_dummies(test_data['Deck'])['E']
    test_data['Deck_F']=pd.get_dummies(test_data['Deck'])['F']
    
    test_data.drop(['PassengerId', 'Pclass','Name','Sex','SibSp','Parch','Ticket','Fare',
                'Cabin','count','Embarked','Title','Deck'], axis=1, inplace=True)
    
    # Scaling
    
    test_data['Age_minmax'] = minmax_scale(test_data['Age'])
    test_data['NameLength_minmax'] = minmax_scale(test_data['NameLength'])
    test_data['NumRelatives_minmax'] = minmax_scale(test_data['NumRelatives'])
    test_data['FarePerPerson_minmax'] = minmax_scale(test_data['FarePerPerson'])
    
    test_data.drop(['Age', 'NameLength','FarePerPerson','NumRelatives'], axis=1, inplace=True)
    
    test_data = test_data.to_numpy()
    
    return(test_data)