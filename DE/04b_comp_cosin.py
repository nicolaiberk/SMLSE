#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Measuring Rhetorical Similarity with Supervised Machine Learning

Estimate Cosin and Jaccard similarity

@author: Nicolai Berk
"""

import os
from datetime import datetime
import pandas as pd
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
import nltk
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style('darkgrid')

df = pd.read_csv('smlse/DE_text.csv')
dt = str(datetime.now().strftime("%Y%m%d-%H%M"))

#%% estimate cosin similarity of each speech to all AfD speeches

# vectorize data
vec_final = TfidfVectorizer(max_df=.5, min_df=5, lowercase=False, ngram_range=(1,1)) # exclude 5 (was 200) least-used terms, similar to Petersen & Spirling (used 5 in crossval bc restricted feature set)
dtm = vec_final.fit_transform([t for t in df.loc[:, 'raw']])
dtm_afd = vec_final.transform([t for t in df.loc[df.party=='AfD', 'raw']])

# estimate cosine similarity two different ways
cosim_sets = cosine_similarity(dtm, dtm_afd)
cosim_full = cosine_similarity(dtm)

# take row mean of similarity to all afd speeches as measure of average similarity
df['cosine_sim'] = cosim_sets.mean(axis=1)
df['cosine_sim_alt'] = cosim_full[:,df.party=='AfD'].mean(axis=1)
np.corrcoef(df.cosine_sim_alt, df.cosine_sim) # measures are identical

# calculate correlation
np.corrcoef(df.afd_pred, df.cosine_sim) # -0.17
np.corrcoef(np.log(df.afd_pred), df.cosine_sim) -0.36

# plot correlation
sns.jointplot(df.cosine_sim, df.afd_pred).savefig('vis/cosin_corr.png')
plt.clf()
sns.jointplot(df.cosine_sim, np.log(df.afd_pred)).savefig('vis/cosin_log_corr.png')
plt.clf()
# plot by party
sns.violinplot(df.party, df.cosine_sim).figure.savefig('vis/cosin_party.png')
plt.clf()
sns.boxplot(df.party, df.cosine_sim).figure.savefig('vis/cosin_party_box.png')
plt.clf()


#%% alternative approach: create mean afd speech, estimate similarity of all speeches
sp_ideal = dtm_afd.mean(axis=0)
df['cosine_sim_alt2'] = cosine_similarity(dtm, sp_ideal)
np.corrcoef(df.cosine_sim_alt2, df.afd_pred) # -0.19
np.corrcoef(df.cosine_sim_alt2, np.log(df.afd_pred)) # -0.36, both v similar to above

# plot correlation
sns.jointplot(df.cosine_sim_alt2, df.afd_pred).savefig('vis/cosin_alt_corr.png')
plt.clf()
sns.jointplot(df.cosine_sim_alt2, np.log(df.afd_pred)).savefig('vis/cosin_alt_log_corr.png')
plt.clf()

# plot by party
sns.violinplot(df.party, df.cosine_sim_alt2).figure.savefig('vis/cosin_alt_party.png')
plt.clf()
sns.boxplot(df.party, df.cosine_sim_alt2).figure.savefig('vis/cosin_alt_party_box.png')
plt.clf()


#%% jaccard similarity
df_tokens = np.array([nltk.word_tokenize(t) for t in df.loc[:, 'raw']])

# from https://medium.com/@adriensieg/text-similarities-da019229c894
def jaccard_similarity(doc1, doc2):
    intersection = set(doc1).intersection(set(doc2))
    union = set(doc1).union(set(doc2))
    return len(intersection)/len(union)

# calculate use of words similar to afd speeches for each speech, relative to overall n of words
    ## this takes ages
df['jaccard']=0
for i in range(len(df)):
    df.loc[i,'jaccard'] = np.mean([jaccard_similarity(sp_afd, df_tokens[i]) for sp_afd in df_tokens[df['party']=='AfD']])

print(np.corrcoef(df.afd_pred, df.jaccard))

# plot correlation
sns.jointplot(df.jaccard, df.afd_pred).savefig('vis/jaccard_corr.png')
plt.clf()
sns.jointplot(df.jaccard, np.log(df.afd_pred)).savefig('vis/jaccard_log_corr.png')
plt.clf()

# plot by party
sns.violinplot(df.party, df.jaccard).figure.savefig('vis/jaccard_party.png')
plt.clf()
sns.boxplot(df.party, df.jaccard).figure.savefig('vis/jaccard_party_box.png')
plt.clf()


# write to csv
df.to_csv('sims/DE_similarities.csv')
