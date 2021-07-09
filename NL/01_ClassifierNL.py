#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Measuring Rhetorical Similarity with Supervised Machine Learning

Classifiers Dutch Tweede Kamer Speeches

@author: Nicolai Berk
"""

# suppress warnings
import warnings
warnings.filterwarnings("ignore")

# Setup
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
import eli5

# load and prepare data
df = pd.read_csv('processed/TweedeKamer_cleaned.csv')
df.party=df.party.fillna('')
df.party[df.party=="other"] = ""
df['date_tr'] = pd.to_datetime(df.date, format='%Y-%m-%d')


## define party families
df.loc[:,'family'] = ''
df.loc[df.party.isin(['FvD','LPF','PVV']),'family'] = 'RR'
df.loc[df.party.isin(['VVD','D66']),'family'] = 'LIB'
df.loc[df.party == 'PvdA','family'] = 'SD'
df.loc[df.party.isin(['GL','DENK','SP','PvdD']),'family'] = 'Left'
df.loc[df.party.isin(['CU','SGP','GPV','RPF']),'family'] = 'OP'
df.loc[df.party == 'CDA','family'] = 'CD'
print(df.family.value_counts())
print(df.party.value_counts())





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

# drop 111 empty speeches
df=df.dropna(subset=['raw'])



#%% train classifiers (once for every legislative session, once for each party family)
logreg = LogisticRegression(max_iter=1000)
for pf in np.unique(df.family):
    df.loc[:,str(pf+'_pred')] = None
    
    

## classifier families
familynames = np.unique(df.family[df.family != ""])

for p in familynames:
    for s in np.unique(df.session):
        print(f'Classifier {p}, session #{s}')
        n_speeches = len(df.loc[(df.session==s) & (df.family == p)])
        if n_speeches<100:
            print(f'<100 {p} speeches in session #'+str(s)+f' ({n_speeches} total), skipping.')
            continue
        vec=TfidfVectorizer(max_df=.5, min_df=5, lowercase=False)
        dtm=vec.fit_transform([t for t in df.loc[df['session']==s,'raw']])

        # choose largest subset for oversampling
        sp_list=[]
        for pf in np.unique(df.family[((df.session==s)&(df.family!=p)&(df.family != ""))]):
            sp_list.append(len(df[(df['family']==pf)&(df.session==s)]))
        sp_max=max(sp_list)

        # equal samples of each party within a party family
        strat={pf_t:sp_max for pf_t in np.unique(df.family[(df.session==s)&(df.family!=p)&(df.family != "")])}
        strat[p]=sp_max*(len(sp_list)-1)
        print('\tSampling strategy:')
        [print(f'\t\t{k}: {strat[k]} speeches') for k in strat]
        sm = SMOTE(random_state=42, sampling_strategy=strat)

        X_final, y_res_pt = sm.fit_resample(dtm, df.loc[df['session']==s, 'family'])
        y=[t==p for t in y_res_pt]

        logreg.fit(X_final, y)
        df.loc[df['session']==s, f'{p}_pred']=[pr[1] for pr in logreg.predict_proba(dtm)]
        
        # export best predictor words
        htmlobj=eli5.show_weights(logreg, top = 31, vec = vec)
        with open(f'vis/eli5_weights_nl_clf_{p}_{s}.htm','wb') as f:
            f.write(htmlobj.data.encode("UTF-8"))
        
        print(f'Finished training classifier {p} session #{s}')
        
        
#%% estimates per party
partynames = ['CDA', 'VVD']

logreg = LogisticRegression(max_iter=1000)
for p in partynames:
    df.loc[:,str(p+'_pred')] = None




## classifier
for p in partynames:
    for s in np.unique(df.session):
        print(f'Classifier {p}, session #{s}')
        n_speeches = len(df.loc[(df.session==s) & (df.party == p)])
        if n_speeches<100:
            print(f'\t<100 {p} speeches in session #'+str(s)+f' ({n_speeches} total), skipping.')
            continue
        vec=TfidfVectorizer(max_df=.5, min_df=5, lowercase=False)
        dtm=vec.fit_transform([t for t in df.loc[df['session']==s,'raw']])

        # choose largest subset for oversampling
        sp_list=[]
        for pt in np.unique(df.party[((df.session==s)&(df.party!=p)&(df.party!=""))]):
            sp_list.append(len(df[(df.party==pt)&(df.session==s)]))
        sp_max=max(sp_list)

        # equal samples of each party within a party
        strat={pt_t:sp_max for pt_t in np.unique(df.party[(df.session==s)&(df.party!=p)&(df.party!="")])}
        strat[p]=sp_max*(len(sp_list)-1)
        print('\tSampling strategy:')
        [print(f'\t\t{k}: {strat[k]} speeches') for k in strat]
        sm = SMOTE(random_state=42, sampling_strategy=strat)

        X_final, y_res_pt = sm.fit_resample(dtm, df.loc[df['session']==s, 'party'])
        y=[t==p for t in y_res_pt]

        logreg.fit(X_final, y)
        df.loc[df['session']==s, f'{p}_pred']=[pr[1] for pr in logreg.predict_proba(dtm)]
        
        # export best predictor words
        htmlobj=eli5.show_weights(logreg, top = 31, vec = vec)
        with open(f'vis/eli5_weights_nl_clf_{p}_{s}.htm','wb') as f:
            f.write(htmlobj.data.encode("UTF-8"))
        
        print(f'Finished training classifier {p} session #{s}\n')

        
        
        
print('Write to csv...')
df_s = df[['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'family', 'raw',
           'RR_pred', 'SD_pred', 'CD_pred', 'LIB_pred', 'Left_pred', 'OP_pred', 
           'CDA_pred', 'VVD_pred', 'raw', 'n_words', 'session']]
df_s.to_csv('smlse/NL_text.csv')
# notext
df_s = df[['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'family', 
           'RR_pred', 'SD_pred', 'CD_pred', 'LIB_pred', 'Left_pred', 'OP_pred', 
           'CDA_pred', 'VVD_pred', 'n_words', 'session']]
df_s.to_csv('smlse/NL_notext.csv')



print('Finished.')
