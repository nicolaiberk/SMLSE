#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Measuring Rhetorical Similarity with Supervised Machine Learning

Best Predictors Dutch Radical Right

@author: Nicolai Berk
"""

# suppress warnings
import warnings
warnings.filterwarnings("ignore")

# Setup
import numpy as np
import pandas as pd
import os
from datetime import datetime as dt
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from imblearn.over_sampling import SMOTE

os.chdir("C:/Users/nicol/Dropbox/Studium/Amsterdam/Studies/Semester 4/Master Thesis/GitHub/SMLSE/NL")

df = pd.read_csv('smlse/NL_text.csv')

#%% run classifier for last session

## subset data
df = df[df.session==7]
df.family = df.family.fillna('')
df = df[df.family != ''] # drop non-parliamentary members' speeches


## define classifier and vectorizer
logreg = LogisticRegression(max_iter=1000)

vec=TfidfVectorizer(max_df=.5, min_df=5, lowercase=False, ngram_range=(1,1)) 
dtm=vec.fit_transform([t for t in df.raw])

# choose largest subset for oversampling
familynames = df.family.unique()
sp_list=[]
for pf in familynames:
    sp_list.append(sum(df['family']==pf))

sp_max=max(sp_list)

# equal samples of each party within a party family
strat={pf_t:sp_max for pf_t in familynames}
strat['RR']=sp_max*(len(sp_list)-1)
sm = SMOTE(random_state=42, sampling_strategy=strat)

X_final, y_res_pt = sm.fit_resample(dtm, df.family)
y=[t=='RR' for t in y_res_pt]

## fit
logreg.fit(X_final, y)

## get weights
feat_wgts = logreg.coef_
feat_names = vec.get_feature_names_out()

fw_df = pd.DataFrame({'feature':feat_names, 'weight':feat_wgts[0]})

## save weights
fw_df.to_csv('smlse/NL_weights.csv', index=False)