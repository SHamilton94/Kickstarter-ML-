# -*- coding: utf-8 -*-
"""KickstarterDataScienceML.ipynb
Automatically generated by Colaboratory.
Original file is located at
    https://colab.research.google.com/github/SHamilton94/Projects/blob/master/KickstarterDataScienceML.ipynb
"""
from __future__ import absolute_import, division, print_function, unicode_literals
import pandas as pd
from sklearn.model_selection import train_test_split
#from google.colab import files
#uploaded=files.upload()
import io
import matplotlib.pyplot as plt
import numpy as np
from IPython.display import clear_output
from six.moves import urllib
import tensorflow as tf
#import tensorflow.compat.v2.feature_column as fc
import csv


#for fn in uploaded.keys():
#  print('User uploaded file "{name}" with length {length} bytes'.format(name=fn, length=len(uploaded[fn])))
dataset='Kickstarter5.csv'

data = pd.read_csv(dataset, index_col=None, header=0, engine='python', na_values=[] )
data.fillna('', inplace=True)

print(data.head())
y=data.state
X=data.drop('state', axis=1)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2)
print("\nX_train:\n")
print(X_train.head())
print(X_train.shape)

print("\nX_test:\n")
print(X_test.head())
print(X_test.shape)

X_train.describe()

CATEGORICAL_COLS = ['name', 'maincategory', 'currency', 'deadline', 'launched']
NUMERIC_COLS = ['goal', 'pledged', 'backers']
feature_columns = []
for feature_name in CATEGORICAL_COLS:
  vocab = X_train[feature_name].unique()
  feature_columns.append(tf.feature_column.categorical_column_with_vocabulary_list(feature_name, vocab))
for feature_name in NUMERIC_COLS:
  feature_columns.append(tf.feature_column.numeric_column(feature_name, dtype=tf.float64))

def make_input_function(data_df, label_df, num_epochs=50, shuffle=True, batch_size=50):
  def input_function():
    ds = tf.data.Dataset.from_tensor_slices((dict(data_df), label_df))  # create tf.data.Dataset object with data and its label
    if shuffle:
      ds=ds.shuffle(1250)
    ds = ds.batch(batch_size).repeat(num_epochs)
    return ds
  return input_function

train_input_fn = make_input_function(X_train, y_train)
test_input_fn = make_input_function(X_test, y_test, num_epochs=1, shuffle=False)

X_train.dtypes

linear_est = tf.estimator.LinearClassifier(feature_columns=feature_columns)

linear_est.train(train_input_fn)  # train

result = linear_est.evaluate(test_input_fn)  # get model metrics/stats by testing on tetsing data

clear_output()  # clears consoke output
print(result['accuracy'])  # the result variable is simply a dict of stats about our model
print(result)

result = list(linear_est.predict(test_input_fn))
print(X_test.iloc[51])
print(y_test.iloc[51])
print(result[51]['probabilities'][1])