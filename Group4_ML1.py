#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 29 2017

@author: Miram
"""

import os
import pandas as pd
from sklearn.cross_validation import train_test_split
from sklearn.preprocessing import scale
from sklearn import neighbors, linear_model
from sklearn.cross_validation import KFold, cross_val_score
from sklearn.metrics import classification_report

os.chdir("/Users/Miram/Desktop/Machine Learning/Assignments/Assignment 1/data")

# 1. Load the data file
red_wine = pd.read_csv("winequality-red.csv", sep=';')

# 2. Construct a new binary column indicating whether the wine is good or not
min(red_wine['quality']) #3
max(red_wine['quality']) #8
# the original wine quality ranges between 3 and 8
original_outcome = red_wine['quality'].values
# wine is labeled as good if having a quality of 6 or higher
good_wine = original_outcome >= 6 # this converts values to either true or false
# convert to 0 and 1
good_wine = good_wine*1

# 3. Split the data into random training and test subsets
predictors = red_wine.ix[:, red_wine.columns != 'quality'].values
X_train, X_test, y_train, y_test = train_test_split(predictors, good_wine,
                                                    test_size=0.5,
                                                    random_state=42)
X_train
X_test
y_train
y_test

# 4. Normalize the data

# We plot the histograms of all the columns in our data set
pd.DataFrame.hist(red_wine, figsize=[15,15])

# We notice the outliers in our data: few samples with very good quality = 8
# and few samples with very poor quality = 3.
# This issue has been taken care of using the new binary variable which classifies
# the data as either good or bad.
# Another issue is the different scales used to measure these variables;
# take for example chlorides (ranges between 0.012 and 0.6)
# and total sulfur dioxide (ranges between 6 and 289).
# As a result, we need to scale/standardize the input data.
input_cols = list(red_wine.ix[:, red_wine.columns != 'quality'].columns)
for col in input_cols:
    red_transform[col + '_zscore'] = (red_wine[col] - red_wine[col].mean()) / red_wine[col].std()

predictors_z = red_transform.values

Z_train, Z_test, y_train, y_test = train_test_split(predictors_z, good_wine,
                                                    test_size=0.5,
                                                    random_state=42)

# The data is now a standard normal distribution centered around zero (mean = 0)
# with standard deviation = 1.

# 5. Train the k-Nearest Neighbours classifiers
acc_dict = dict()
k_range = np.arange(1, 500, 5)
for i in k_range:
    knn = neighbors.KNeighborsClassifier(n_neighbors=i)
    model = knn.fit(Z_train, y_train)
    accuracy = model.score(Z_test, y_test)
    acc_dict[i] = round(float(accuracy),3)
# compute the accuracy for each k
acc_dict
# k with highest accuracy
max(acc_dict, key=acc_dict.get) # k = 86

# 6. Evaluate each classifier using 5-fold cross validation
kfold = KFold(1599, n_folds=5)
for iteration, data in enumerate(kfold, start=1):
    print(iteration, data[0], data[1])

# 5-fold cross validation with k = 5
# use classification accuracy as the evaluation metric
# find the optimal k
scores_dict = dict()
for i in k_range:
    knn = neighbors.KNeighborsClassifier(n_neighbors=i)
    scores = cross_val_score(knn, predictors_z, good_wine, cv=5, scoring='accuracy')
    #print(scores)
    #print(scores.mean())
    scores_dict[i] = round(float(scores.mean()),3)

# We also get k=86!
scores_dict[86] # Cross-Validated accuracy for optimal k

# 7. Predict the generalisation error using the test data set
knn = neighbors.KNeighborsClassifier(n_neighbors=86)
model = knn.fit(Z_train, y_train)
y_true, y_pred = y_test, model.predict(Z_test)
print(classification_report(y_true, y_pred))