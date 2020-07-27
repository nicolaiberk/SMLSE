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
from sklearn.linear_model import LogisticRegression
import time


df = pd.read_csv('processed/Bundestag_cleaned.csv')

df.party = df.party.fillna('')
df = df[df.party != ''] # drop non-partisan/non parliamentary members' speeches

# subset similar to Wordfish
df['date_tr'] = pd.to_datetime(df.date, format='%Y-%m-%d')
df = df[df['date_tr'] > pd.to_datetime('2017-10-24', format='%Y-%m-%d')] # subset for speeches after afd entrance
df = df[df['n_words_raw'] > 50]

# balanced subset of 1000 speeches

afd_speeches = df[df.party == 'AfD'].sample(500)
spd_speeches = df[df.party == 'SPD'].sample(100)
cdu_speeches = df[df.party == 'CDU/CSU'].sample(100)
fdp_speeches = df[df.party == 'FDP'].sample(100)
gru_speeches = df[df.party == 'GRUENE'].sample(100)
lnk_speeches = df[df.party == 'PDS/LINKE'].sample(100)

df_sub = pd.concat([afd_speeches, spd_speeches, cdu_speeches, 
                    fdp_speeches, gru_speeches, lnk_speeches],
                   axis=0,ignore_index=True)


#%% train on subset
            
# fit
vec_final = TfidfVectorizer(max_df=.5, min_df=5, lowercase=False, ngram_range=(1,1)) 
dtm_sub = vec_final.fit_transform([t for t in df_sub['raw']])
dtm_full = vec_final.transform([t for t in df['raw']])
logreg = LogisticRegression(max_iter = 1000)

y_res=[t=='AfD' for t in df_sub['party']]

start_time = time.time() # time execution for comparison with wordfish
logreg.fit(dtm_sub, y_res)

# predict
pred = logreg.predict_proba(dtm_full)
l_pred=[]
for p in pred:
    l_pred.append(p[1])

end_time = time.time()
diff = f"{(end_time - start_time)} seconds"

# transform into meaningful time
start_time = time.strftime('%h-%m-%s', time.localtime(start_time))
end_time = time.strftime('%h-%m-%s', time.localtime(end_time))

# write runtime
text_file = open("runtime_smlse1000.txt", "w")
text_file.write(f'Start time: {start_time},\n end time {end_time},\n difference: {diff}')
text_file.close()

df['afd_pred'] = l_pred
    

# predict afd|independent
y_afd_ind=[(t in ['AfD', 'independent']) for t in df_sub['party']]
logreg.fit(dtm_sub, y_afd_ind)

pred = logreg.predict_proba(dtm_full)
l_pred=[]
for p in pred:
    l_pred.append(p[1])
    
df['afd_ind_pred'] = l_pred


df['afd'] = (df.party=='AfD')
df['afd_ind'] = (df.party.isin(['AfD', 'independent']))

# write to csv for plotting in R
df_r = df[['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'afd', 'afd_ind', 'afd_pred', 'afd_ind_pred', 'n_words_raw']]
df_r.to_csv('smlse/DE_smlse1000.csv')
