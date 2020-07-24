import re
import time
import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from imblearn.over_sampling import SMOTE    
from sklearn.preprocessing import MultiLabelBinarizer
from sklearn.linear_model import LogisticRegression

# load data, subset 
logreg = LogisticRegression(max_iter=1000)
df  = pd.read_csv('smlse/NL_text.csv')
df = df[df['session'] == 3] # 2003-2006 to train identical classifier
df.date = [time.strptime(i, '%Y-%m-%d') for i in df.date]
df = df.reset_index()

## define party families
df.loc[:,'family'] = ''
df.loc[df.party.isin(['FvD','LPF','PVV']),'family'] = 'RR'
df.loc[df.party.isin(['VVD','D66']),'family'] = 'Liberal'
df.loc[df.party == 'PvdA','family'] = 'Social Democrat'
df.loc[df.party.isin(['GL','DENK','SP','PvdD']),'family'] = 'Left'
df.loc[df.party.isin(['CU','SGP','GPV','RPF']),'family'] = 'Orthodox Protestant'
df.loc[df.party == 'CDA','family'] = 'Christian Democrat'
print(df.family.value_counts())

# create matrix for relevant speeches
vec = TfidfVectorizer(max_df=.5, min_df=5, lowercase=False)
dtm = vec.fit_transform([t for t in df['raw']])

# choose largest subset for oversampling
sp_list=[]
for pf in np.unique(df.family[(df.family!='RR')]):
    sp_list.append(len(df[(df['family']==pf)]))
sp_max=max(sp_list)
        
# equal samples of each party within a party family
strat={p:sp_max for p in np.unique(df.family)}
strat['RR']=sp_max*(len(sp_list)-1)
sm = SMOTE(random_state=42, sampling_strategy=strat)

X_final, y_res_pt = sm.fit_resample(dtm, df.family)
y=[t=='RR' for t in y_res_pt]

# fit model
logreg.fit(X_final, y)

# subset to relevant time-frame and speaker
df = df[df['date'] < time.strptime('04.09.2004', '%d.%m.%Y')] # preceding Wilders' exit
df = df[[bool(re.match(".*Wilders.*", t)) for t in df.speaker]]
df = df.reset_index()

# transform to dtm again, using old vectorizer to fit coefficients (not oversampled)
dtm = vec.transform([t for t in df['raw']])

# get weight and count of each word                                   
counts = dtm.sum(axis = 0)
coefficients = logreg.coef_
weights = pd.DataFrame(counts)*pd.DataFrame(coefficients)

# show most important positive words
wilders = pd.concat([pd.DataFrame(np.transpose(vec.get_feature_names())),pd.DataFrame(np.transpose(weights))], axis = 1)
wilders.columns = ['word', 'weight']
wilders = wilders.sort_values(by = ['weight'], ascending=False)
print(wilders.head(30))

# write to csv
wilders.to_csv('vis/Wilders_bpw.csv')