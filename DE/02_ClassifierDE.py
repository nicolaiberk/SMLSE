#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  5 15:07:33 2020

@author: Nicolai Berk
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import datetime
from sklearn.feature_extraction.text import TfidfVectorizer
from imblearn.over_sampling import SMOTE
from sklearn.linear_model import LogisticRegression


df = pd.read_csv('processed/Bundestag_cleaned.csv')

df.party = df.party.fillna('')
df = df[df.party != ''] # drop non-partisan/non parliamentary members' speeches

# subset
df['date_tr'] = pd.to_datetime(df.date, format='%Y-%m-%d')
df = df[df['date_tr'] > pd.to_datetime('2017-10-24', format='%Y-%m-%d')] # subset for speeches after afd entrance


#%% train on full set
            
# fit
vec_final = TfidfVectorizer(max_df=.5, min_df=5, lowercase=False, ngram_range=(1,1)) 
dtm = vec_final.fit_transform([t for t in df['raw']])
logreg = LogisticRegression(max_iter = 1000)

sm = SMOTE(random_state=42, 
             sampling_strategy={'AfD':len(df[df['party']=='CDU/CSU'])*5,
                                'SPD':len(df[df['party']=='CDU/CSU']),
                                'CDU/CSU':len(df[df['party']=='CDU/CSU']),
                                'FDP':len(df[df['party']=='CDU/CSU']),
                                'GRUENE':len(df[df['party']=='CDU/CSU']),
                                'PDS/LINKE':len(df[df['party']=='CDU/CSU'])})
X_final, y_res_pt = sm.fit_resample(dtm, df['party'])
y_res=[t=='AfD' for t in y_res_pt]
logreg.fit(X_final, y_res)


# predict
pred = logreg.predict_proba(dtm)
l_pred=[]
for p in pred:
    l_pred.append(p[1])

df['afd_pred'] = l_pred


#%% assess performance

# inspect classifier using eli5
import eli5
htmlobj=eli5.show_weights(logreg, top = 30, vec = vec_final)
with open('vis/eli5_weights_de_clf.htm','wb') as f: 
    f.write(htmlobj.data.encode("UTF-8"))

htmlobj=eli5.show_prediction(logreg, df[df['speaker']=='Alexander Gauland']['raw'].iloc[2], vec = vec_final)
with open('vis/eli5_example_de_gauland.htm','wb') as f: 
    f.write(htmlobj.data.encode("UTF-8"))


# predict afd|independent
y_afd_ind=[(t in ['AfD', 'independent']) for t in y_res_pt]
logreg.fit(X_final, y_afd_ind)

pred = logreg.predict_proba(dtm)
l_pred=[]
for p in pred:
    l_pred.append(p[1])
    
df['afd_ind_pred'] = l_pred


df['afd'] = (df.party=='AfD')
df['afd_ind'] = (df.party.isin(['AfD', 'independent']))

#%% write to csv

# no text
df_r = df[['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'afd', 'afd_ind', 'afd_pred', 'afd_ind_pred', 'n_words_raw']]
df_r.to_csv('smlse/DE_notext.csv')

# with text
df = df[['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'afd', 'afd_ind', 'afd_pred', 'afd_ind_pred', 'n_words_raw', 'raw']]
df.to_csv('smlse/DE_text.csv')


#%% train classifiers per party, export best predictor words for each party
from sklearn.linear_model import LinearRegression
clfs={k: LinearRegression() for k in ['AfD', 'CDU/CSU', 'FDP', 'GRUENE', 'PDS/LINKE', 'SPD']}
vecs={k: TfidfVectorizer(max_df=.5, min_df=5, lowercase=False) for k in ['AfD', 'CDU/CSU', 'FDP', 'GRUENE', 'PDS/LINKE', 'SPD']}

for pt in ['AfD', 'CDU/CSU', 'FDP', 'GRUENE', 'PDS/LINKE', 'SPD']:
    y = df['afd_pred'][df['party']==pt]
    X = vecs[pt].fit_transform(df['raw'][df['party']==pt])
    clfs[pt].fit(X,y)
    
    # export estimation of best predictor words:
    htmlobj=eli5.show_weights(clfs[pt], top = 30, vec = vecs[pt])
    with open(basedir+'vis/eli5_weights_de_'+pt[0:3]+'.htm','wb') as f: 
        f.write(htmlobj.data.encode("UTF-8"))
