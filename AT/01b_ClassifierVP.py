#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  12 10:07:33 2020

@author: Nicolai Berk
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
from datetime import datetime as dt
from sklearn.feature_extraction.text import TfidfVectorizer
from imblearn.over_sampling import SMOTE    
from sklearn.linear_model import LogisticRegression

df = pd.read_csv('smlse/AT_text.csv')

#%% prepare 
df['ÖVP'] = (df.party=='ÖVP')
df['date_tr'] = pd.to_datetime(df.date, format='%Y-%m-%d')

# number of positive cases
print('Speeches ÖVP: ', sum(df.ÖVP)) # 24k
print('Relative: ', sum(df.ÖVP)/len(df)) # 28%


#%% train classifiers (once for every legislative session)
dtms = {s:'' for s in np.unique(df.session)}
vecs = {s:'' for s in np.unique(df.session)}
logreg = LogisticRegression(max_iter=1000)
df.loc[:,'ÖVP_pred'] = None

                
print('Training classifiers ÖVP:')
for s in np.unique(df.session):
    print('\tSession #'+str(s)+'...')
    if len(np.unique(df.loc[df['session']==s,'ÖVP']))==1:
        print('No '+pt+' speeches in session #'+str(s)+', skipping.')
        continue
    vecs[s]=TfidfVectorizer(max_df=.5, min_df=5, lowercase=False) # was 200 least-used terms, similar to Petersen & Spirling
    dtms[s]=vecs[s].fit_transform([t for t in df.loc[df['session']==s,'raw']])

    # choose largest subset for oversampling
    sp_list=[]
    for p in np.unique(df.party[(df.session==s)&(df.party!='independent')]):
        sp_list.append(len(df[(df['party']==p)&(df.session==s)]))
    sp_max=max(sp_list)

    strat={p:sp_max for p in np.unique(df.party[(df.party!='ÖVP') & (df.party!='independent') & (df.session==s)])}
    strat['ÖVP']=sp_max*len(sp_list)
    
    sm = SMOTE(random_state=42, 
         sampling_strategy=strat)

    X_final, y_res_pt = sm.fit_resample(dtms[s], df.loc[df['session']==s, 'party'])
    plt.hist(y_res_pt)
    y=[t=='ÖVP' for t in y_res_pt]

    logreg.fit(X_final, y)
    df.loc[df['session']==s, str('ÖVP_pred')]=[pr[1] for pr in logreg.predict_proba(dtms[s])]
    print('\tFinished training classifier '+' session #'+str(s))

#%%  VP general predictor
# drop non-classified families ('', independent, PILZ, Stronach)
df = df.loc[df.family!='',:]


#%% write into csv

df_r = df[['date', 'id', 'party', 'partyfacts', 'family', 'session', 'speaker', 'agenda', 
           'BZÖ', 'FPÖ','ÖVP', 'BZÖ_pred', 'FPÖ_pred', 'RR_pred', 'ÖVP_pred', 'n_words']]
df_r.to_csv('smlse/AT_notext.csv')

# with text
df = df[['date', 'id', 'party', 'partyfacts', 'family', 'session', 'speaker', 'agenda', 
         'BZÖ', 'FPÖ','ÖVP', 'BZÖ_pred', 'FPÖ_pred', 'RR_pred', 'ÖVP_pred', 'raw', 'n_words']]
df.to_csv('smlse/AT_text.csv')

#%% assess best predictor words for last session
import eli5
htmlobj=eli5.show_weights(logreg, top = 30, vec = vecs[16])
with open('vis/eli5_weights_at_vp.htm','wb') as f:   # Use some reasonable temp name
    f.write(htmlobj.data.encode("UTF-8"))
