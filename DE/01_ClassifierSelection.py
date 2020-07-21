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

df = pd.read_csv('processed/Bundestag_cleaned.csv')

df.party=df.party.fillna('')
df=df[df.party!=''] # drop non-parliamentary members' speeches

# subset
df['date_tr'] = pd.to_datetime(df.date, format='%Y-%m-%d')
df = df[df['date_tr']>pd.to_datetime('2017-10-24', format='%Y-%m-%d')] # subset for speeches after afd entrance


#%% vectorize data

# define crossvalidation samples
df['sample']=[np.random.randint(0,5) for i in range(len(df))]
plt.hist(df['sample'])

## vectorize
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer

vecs = {'tfidf':{'raw':TfidfVectorizer(max_df=.5, min_df=5, lowercase=False),
                 'stems':TfidfVectorizer(max_df=.5, min_df=5, lowercase=False)},
        'count':{'raw':CountVectorizer(max_df=.5, min_df=5, lowercase=False),
                 'stems':CountVectorizer(max_df=.5, min_df=5, lowercase=False)}}


dtms = {'train':{'tfidf':{'raw':{k:'' for k in range(5)},
                          'stems':{k:'' for k in range(5)}},
                 'count':{'raw':{k:'' for k in range(5)},
                          'stems':{k:'' for k in range(5)}}},
        'test':{'tfidf':{'raw':{k:'' for k in range(5)},
                        'stems':{k:'' for k in range(5)}},
                'count':{'raw':{k:'' for k in range(5)},
                         'stems':{k:'' for k in range(5)}}}}



for vec in ['tfidf', 'count']:
    for text in ['raw', 'stems']:
        for s in range(5):
            dtms['train'][vec][text][s]=vecs[vec][text].fit_transform([t for t in df[text][df['sample']!=s]])
            dtms['test'][vec][text][s]=vecs[vec][text].transform([t for t in df[text][df['sample']==s]])
            



#%% oversampling
from imblearn.over_sampling import SMOTE    

y = ['']*5

X = {'raw':{'tfidf':{k:'' for k in range(5)}, 'count':{k:'' for k in range(5)}},
     'stems':{'tfidf':{k:'' for k in range(5)}, 'count':{k:'' for k in range(5)}}}
y_res_pt = {k:'' for k in range(5)}



# balance party samples
for s in range(5):
    sm = SMOTE(random_state=42, 
                 sampling_strategy={'AfD':len(df[(df['party']=='CDU/CSU') & (df['sample']!=s)])*5,
                                    'SPD':len(df[(df['party']=='CDU/CSU') & (df['sample']!=s)]),
                                    'CDU/CSU':len(df[(df['party']=='CDU/CSU') & (df['sample']!=s)]),
                                    'FDP':len(df[(df['party']=='CDU/CSU') & (df['sample']!=s)]),
                                    'GRUENE':len(df[(df['party']=='CDU/CSU') & (df['sample']!=s)]),
                                    'PDS/LINKE':len(df[(df['party']=='CDU/CSU') & (df['sample']!=s)])})
    for vec in ['tfidf', 'count']:
        for text in ['raw', 'stems']:
            y[s] = df['party'][df['sample']!=s]
            X[text][vec][s], y_res_pt[s] = sm.fit_resample(dtms['train'][vec][text][s], y[s])
            plt.hist(y_res_pt[s])


#%% train classifiers

from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import MultinomialNB
from sklearn import svm
import sklearn.metrics


logreg = LogisticRegression(max_iter = 1000)
naive = MultinomialNB()
svm_m = svm.LinearSVC()
models = [logreg, naive, svm_m]

# define outcome category
df["['AfD']"] = False
df.loc[df['party']=='AfD', "['AfD']"] = True

df["['AfD', 'independent']"] = False
df.loc[df['party']=='AfD', "['AfD', 'independent']"] = True
df.loc[df['party']=='independent', "['AfD', 'independent']"] = True


y_res = {"['AfD']":{k:'' for k in range(5)},
        "['AfD', 'independent']":{k:'' for k in range(5)}}

for parties in [["AfD"], ["AfD", "independent"]]:
    for s in range(5):
        y_res[str(parties)][s]=[(t in parties) for t in y_res_pt[s]]

for m in models:
    for parties in [["AfD"], ["AfD", "independent"]]:
        for vec in ['tfidf', 'count']:
            for text in ['raw', 'stems']:
                acc    = []
                precis = []
                recall = []
                f1     = []
                for i in range(5):
                    m.fit(X[text][vec][i], y_res[str(parties)][i])
                    prediction = m.predict(dtms['test'][vec][text][i])
                    acc.append(sklearn.metrics.accuracy_score([t for t in df[str(parties)][df['sample']==i]], prediction))
                    precis.append(sklearn.metrics.precision_score([t for t in df[str(parties)][df['sample']==i]], prediction, pos_label=True))
                    recall.append(sklearn.metrics.recall_score([t for t in df[str(parties)][df['sample']==i]], prediction, pos_label=True))
                    f1.append(sklearn.metrics.f1_score([t for t in df[str(parties)][df['sample']==i]], prediction, pos_label=True))
                print(str(m) + ', ' + str(vec) + ', '+str(text) + ', '+str(parties) + ':\n' +
                      '\nAverage accuracy: ' + str(np.mean(acc)) +
                      '\nAverage precision: ' + str(np.mean(precis)) +
                      '\nAverage recall: ' + str(np.mean(recall)) +
                      '\nAverage f1-score: ' + str(np.mean(f1)) + '\n\n\n')

# similar performance across models, but best performance of LogReg, raw text, tfidf vectorizer 
#   (accuracy: 0.89, precision: 0.62, recall: 0.59, F1: 0.61)
                
# might add a plot/table for appendix showing performance of each classifier
