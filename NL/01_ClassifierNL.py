#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  12 12:53:33 2020

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
import sklearn.metrics
import math
import re

df = pd.read_csv('processed/TweedeKamer_cleaned.csv')
df.party=df.party.fillna('')

## define party families
df.loc[:,'family'] = ''
df.loc[df.party.isin(['FvD','LPF','PVV']),'family'] = 'RR'
df.loc[df.party.isin(['VVD','D66']),'family'] = 'Liberal'
df.loc[df.party == 'PvdA','family'] = 'Social Democrat'
df.loc[df.party.isin(['GL','DENK','SP','PvdD']),'family'] = 'Left'
df.loc[df.party.isin(['CU','SGP','GPV','RPF']),'family'] = 'Orthodox Protestant'
df.loc[df.party == 'CDA','family'] = 'Christian Democrat'
print(df.family.value_counts())


#%% prepare data

df['LPF'] = (df.party=='LPF')
df['PVV'] = (df.party=='PVV')
df['FvD'] = (df.party=='FvD')

radright = ['LPF','PVV','FvD']
df['RR'] = (df.party.isin(radright))

df['date_tr'] = pd.to_datetime(df.date, format='%Y-%m-%d')

# number of positive cases
print('Speeches LPF: ', sum(df.LPF))
print('Relative: ', sum(df.LPF)/len(df))

print('Speeches PVV: ', sum(df.PVV))
print('Relative: ', sum(df.PVV)/len(df))

print('Speeches FvD: ', sum(df.FvD))
print('Relative: ', sum(df.FvD)/len(df))

print('Speeches RR: ', sum(df.RR))
print('Relative: ', sum(df.RR)/len(df))


# add indicators for legislative session
df['session'] = None
df.loc[(df['date_tr']>dt.strptime('3 May 1994', '%d %B %Y')), 
       'session'] = 0
df.loc[(df['date_tr']>dt.strptime('6 May 1998', '%d %B %Y')), 
       'session'] = 1
df.loc[(df['date_tr']>dt.strptime('15 May 2002', '%d %B %Y')),
       'session'] = 2
df.loc[(df['date_tr']>dt.strptime('22 January 2003', '%d %B %Y')),
       'session'] = 3
df.loc[(df['date_tr']>dt.strptime('22 November 2006', '%d %B %Y')),
       'session'] = 4
df.loc[(df['date_tr']>dt.strptime('9 June 2010', '%d %B %Y')), 
       'session'] = 5
df.loc[(df['date_tr']>dt.strptime('12 September 2012', '%d %B %Y')),
       'session'] = 6
df.loc[(df['date_tr']>dt.strptime('15 March 2017', '%d %B %Y')),
       'session'] = 7

# drop 111 empty speeches:
df=df.dropna(subset=['raw'])


#%% train classifiers (once for every legislative session, once for each RR party)
dtms = {p:{s:'' for s in np.unique(df.session)} for p in radright}
vecs = {p:{s:'' for s in np.unique(df.session)} for p in radright}

logreg = LogisticRegression(max_iter=1000)
for pt in radright:
    df.loc[:,str(pt+'_pred')] = None


#%% general RR classifier
dtms['RR']={s:'' for s in np.unique(df.session)}
vecs['RR']={s:'' for s in np.unique(df.session)}


## classifier
for s in np.unique(df.session):
        print('\tSession #'+str(s)+'...')
        if len(np.unique(df.loc[df['session']==s,'RR']))==1:
            print('No RR speeches in session #'+str(s)+', skipping.')
            continue
        vecs['RR'][s]=TfidfVectorizer(max_df=.5, min_df=5, lowercase=False)
        dtms['RR'][s]=vecs['RR'][s].fit_transform([t for t in df.loc[df['session']==s,'raw']])
        
        # choose largest subset for oversampling
        sp_list=[]
        for pf in np.unique(df.family[((df.session==s)&(df.family!='RR'))]):
            sp_list.append(len(df[(df['family']==pf)&(df.session==s)]))
        sp_max=max(sp_list)
        
        # equal samples of each party within a party family
        strat={p:sp_max for p in np.unique(df.family[(df.session==s)])}
        strat['RR']=sp_max*(len(sp_list)-1)
        sm = SMOTE(random_state=42, sampling_strategy=strat)
      
        X_final, y_res_pt = sm.fit_resample(dtms['RR'][s], df.loc[df['session']==s, 'family'])
        y=[t=='RR' for t in y_res_pt]
        
        logreg.fit(X_final, y)
        df.loc[df['session']==s, 'RR_pred']=[pr[1] for pr in logreg.predict_proba(dtms['RR'][s])]
        print('\tFinished training classifier RR session #'+str(s))

        

        
        
print('Write to csv...')
df_s = df[['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'family', 'RR', 'RR_pred', 'raw', 'n_words', 'session']]
df_s.to_csv('smlse/NL_text.csv')
# notext
df_s = df[['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'family', 'RR', 'RR_pred', 'n_words', 'session']]
df_s.to_csv('smlse/NL_notext.csv')

#%% assess best predictor words for last session

import eli5
htmlobj=eli5.show_weights(logreg, top = 30, vec = vecs['RR'][7])
with open('vis/eli5_weights_nl_clf.htm','wb') as f:
    f.write(htmlobj.data.encode("UTF-8"))

print('Finished.')