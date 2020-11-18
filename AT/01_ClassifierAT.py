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
import eli5

df = pd.read_csv('processed/Nationalrat_cleaned.csv')
df.party=df.party.fillna('')
# drop 'Schriftfuehrer'
df = df[df.party != '']

## define party families beyond RR
df.loc[:,'family'] = ''
df.loc[df.party.isin(['BZÖ','FPÖ']),'family'] = 'RR'
df.loc[df.party.isin(['NEOS','LIF']),'family'] = 'Liberal'
df.loc[df.party == 'SPÖ','family'] = 'Social Democrat'
df.loc[df.party=='Grüne','family'] = 'Greens'
df.loc[df.party == 'ÖVP','family'] = 'Christian Democrat'
print(df.family.value_counts())


#%% prepare 
df['FPÖ'] = (df.party=='FPÖ')
df['BZÖ'] = (df.party=='BZÖ')
df['ÖVP'] = (df.party=='ÖVP')
df['SPÖ'] = (df.party=='SPÖ')

outcome_pt = ['FPÖ', 'BZÖ', 'ÖVP', 'SPÖ', 'RR']
df['RR'] = (df.party.isin(['FPÖ','BZÖ']))

df.loc[df['party']=='JETZT','party'] = 'PILZ'
df.loc[df['party']=='Jetzt – Liste PILZ','party'] = 'PILZ'


df['date_tr'] = pd.to_datetime(df.date, format='%Y-%m-%d')

# number of positive cases
print('Speeches FPÖ: ', sum(df.FPÖ)) # 17,308
print('Relative: ', sum(df.FPÖ)/len(df)) # 19.2%

# number of positive cases
print('Speeches BZÖ: ', sum(df.BZÖ)) # 4,170
print('Relative: ', sum(df.BZÖ)/len(df)) # 4.6%

# add indicators for legislative session
df['session'] = None
df.loc[(df['date_tr']>=dt.strptime('15.01.1996', '%d.%m.%Y')) &  
       (df['date_tr']<=dt.strptime('28.10.1999', '%d.%m.%Y')),
       'session'] = 10
df.loc[(df['date_tr']>=dt.strptime('29.10.1999', '%d.%m.%Y')) &  
       (df['date_tr']<=dt.strptime('19.12.2002', '%d.%m.%Y')),
       'session'] = 11
df.loc[(df['date_tr']>=dt.strptime('20.12.2002', '%d.%m.%Y')) &  
       (df['date_tr']<=dt.strptime('29.10.2006', '%d.%m.%Y')),
       'session'] = 12
df.loc[(df['date_tr']>=dt.strptime('30.10.2006', '%d.%m.%Y')) &  
       (df['date_tr']<=dt.strptime('27.10.2008', '%d.%m.%Y')),
       'session'] = 13
df.loc[(df['date_tr']>=dt.strptime('28.10.2008', '%d.%m.%Y')) &  
       (df['date_tr']<=dt.strptime('28.10.2013', '%d.%m.%Y')),
       'session'] = 14
df.loc[(df['date_tr']>=dt.strptime('29.10.2013', '%d.%m.%Y')) &  
       (df['date_tr']<=dt.strptime('08.11.2017', '%d.%m.%Y')),
       'session'] = 15
df.loc[(df['date_tr']>=dt.strptime('09.11.2017', '%d.%m.%Y')) &  
       (df['date_tr']<=dt.strptime('22.10.2019', '%d.%m.%Y')),
       'session'] = 16


#%% train classifiers (once for every legislative session, once for each RR party + VP + SP)
dtms = {p:{s:'' for s in np.unique(df.session)} for p in outcome_pt}
vecs = {p:{s:'' for s in np.unique(df.session)} for p in outcome_pt}
logreg = LogisticRegression(max_iter=1000)

                
for pt in outcome_pt[0:4]:
    print('Training classifiers '+pt+':')
    for s in np.unique(df.session):
        print('\tSession #'+str(s)+'...')
        if len(np.unique(df.loc[df['session']==s,pt]))==1:
            print('No '+pt+' speeches in session #'+str(s)+', skipping.')
            continue
        vecs[pt][s]=TfidfVectorizer(max_df=.5, min_df=5, lowercase=False)
        dtms[pt][s]=vecs[pt][s].fit_transform([t for t in df.loc[df['session']==s,'raw']])
        
        # choose largest subset for oversampling
        sp_list=[]
        for p in np.unique(df.party[(df.session==s)&(df.party!='independent')]):
            sp_list.append(len(df[(df['party']==p)&(df.session==s)]))
        sp_max=max(sp_list)
        
        strat={p:sp_max for p in np.unique(df.party[(df.party!=pt) & (df.party!='independent') & (df.session==s)])}
        strat[pt]=sp_max*len(sp_list)
        
        sm = SMOTE(random_state=42, 
             sampling_strategy=strat)
      
        X_final, y_res_pt = sm.fit_resample(dtms[pt][s], df.loc[df['session']==s, 'party'])
        plt.hist(y_res_pt)
        y=[t==pt for t in y_res_pt]
        
        logreg.fit(X_final, y)
        df.loc[df['session']==s, str(pt+'_pred')]=[pr[1] for pr in logreg.predict_proba(dtms[pt][s])]
        htmlobj=eli5.show_weights(logreg, top = 31, vec = vecs[pt][s])
        with open('vis/eli5_weights_at_clf_'+pt+str(s)+'.htm','wb') as f:
            f.write(htmlobj.data.encode("UTF-8"))

        print('\tFinished training classifier '+pt+' session #'+str(s))

#%%  RR general predictor
# drop non-classified families ('', independent, PILZ, Stronach)
df = df.loc[df.family!='',:]


for s in np.unique(df.session):
        print('\tSession #'+str(s)+'...')
        if len(np.unique(df.loc[df['session']==s,'RR']))==1:
            print('No RR speeches in session #'+str(s)+', skipping.')
            continue
        vecs['RR'][s]=TfidfVectorizer(max_df=.5, min_df=5, lowercase=False, ngram_range=(1,1)) # was 200 least-used terms, similar to Petersen & Spirling (used 5 in crossval bc restricted feature set)
        dtms['RR'][s]=vecs['RR'][s].fit_transform([t for t in df.loc[df['session']==s,'raw']])
        
        # choose largest subset for oversampling
        sp_list=[]
        for p in np.unique(df.family[(df.session==s)&(df.family!='RR')]):
            sp_list.append(len(df[(df['family']==p)&(df.session==s)]))
        sp_max=max(sp_list)
        
        strat={p:sp_max for p in np.unique(df.family[(df.family!='RR')&(df.session==s)])}
        strat['RR']=sp_max*(len(sp_list))
        
        sm = SMOTE(random_state=42, 
             sampling_strategy=strat)
      
        X_final, y_res_pt = sm.fit_resample(dtms['RR'][s], df.loc[df['session']==s, 'family'])
        y=[t=='RR' for t in y_res_pt]
        
        logreg.fit(X_final, y)
        df.loc[df['session']==s, 'RR_pred']=[pr[1] for pr in logreg.predict_proba(dtms['RR'][s])]
        print('\tFinished training classifier RR session #'+str(s))

#%% write into csv

df_r = df[['date', 'id', 'party', 'partyfacts', 'family', 'session', 'speaker', 'agenda', 'BZÖ', 'FPÖ', 'SPÖ', 'ÖVP', 'BZÖ_pred', 'FPÖ_pred', 'SPÖ_pred', 'ÖVP_pred', 'RR_pred', 'n_words']]
df_r.to_csv('smlse/AT_notext.csv')

# with text
df = df[['date', 'id', 'party', 'partyfacts', 'family', 'session', 'speaker', 'agenda', 'BZÖ', 'FPÖ', 'SPÖ', 'ÖVP', 'BZÖ_pred', 'FPÖ_pred', 'SPÖ_pred', 'ÖVP_pred', 'RR_pred', 'raw', 'n_words']]
df.to_csv('smlse/AT_text.csv')