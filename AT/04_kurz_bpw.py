#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Measuring Rhetorical Similarity with Supervised Machine Learning

Best predictor words OEVP 2017-18

@author: Nicolai Berk
"""

# Setup
import re
import time
import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LinearRegression

# load data, subset, clean
df  = pd.read_csv('smlse/AT_text.csv')
df.date = [time.strptime(i, '%Y-%m-%d') for i in df.date]
df = df[df.date >= time.strptime('2017-01-01', '%Y-%m-%d')]
df = df[df.party == 'ÖVP']
df = df.reset_index()

# create matrix for relevant speeches
vec = TfidfVectorizer(max_df=.5, min_df=5, lowercase=False)
dtm = vec.fit_transform([t for t in df['raw']])

# fit model
linreg = LinearRegression()
y = df.RR_pred
linreg.fit(dtm, y)

# transform to dtm again, using old vectorizer to fit coefficients (not oversampled)
dtm = vec.transform([t for t in df['raw']])

# get weight and count of each word                                   
counts = np.transpose(dtm.sum(axis = 0))
coefficients = linreg.coef_
weights = pd.DataFrame(counts)*pd.DataFrame(coefficients)

# show most important positive words
vp = pd.concat([pd.DataFrame(np.transpose(vec.get_feature_names())),pd.DataFrame(weights), pd.DataFrame(coefficients), pd.DataFrame(counts)], axis = 1)
vp.columns = ['word', 'weight', 'coef', 'count']
vp = vp.sort_values(by = ['coef'], ascending=False)
print(vp.head(30))

# write to csv
vp.to_csv('vis/vp_bpw.csv')

#%% estimate preceding coalition formation

# subset
df = df[df.date < time.strptime('2017-12-15', '%Y-%m-%d')]

# create matrix for relevant speeches
dtm = vec.fit_transform([t for t in df['raw']])

# fit model
linreg = LinearRegression()
y = df.RR_pred
linreg.fit(dtm, y)

# transform to dtm again, using old vectorizer to fit coefficients (not oversampled)
dtm = vec.transform([t for t in df['raw']])

# get weight and count of each word                                   
counts = np.transpose(dtm.sum(axis = 0))
coefficients = linreg.coef_
weights = pd.DataFrame(counts)*pd.DataFrame(coefficients)

# show most important positive words
vp = pd.concat([pd.DataFrame(np.transpose(vec.get_feature_names())),pd.DataFrame(weights), pd.DataFrame(coefficients), pd.DataFrame(counts)], axis = 1)
vp.columns = ['word', 'weight', 'coef', 'count']
vp = vp.sort_values(by = ['coef'], ascending=False)
print(vp.head(30))

# write to csv
vp.to_csv('vis/vp_bpw_precol.csv')
