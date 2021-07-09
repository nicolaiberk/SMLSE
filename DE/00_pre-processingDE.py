# -*- coding: utf-8 -*-
"""
Measuring Rhetorical Similarity with Supervised Machine Learning

Pre-processing German Bundestag Speeches

@author: Nicolai Berk
"""

#%% Setup
import csv
import datetime
import nltk
from nltk.stem.snowball import SnowballStemmer
from sys import stdout
import os
import re

stemmer = SnowballStemmer('german')

csv.field_size_limit(100000000)

n = 380000 # ~n of sppeeches to process


print('Processing German speeches...')
# stemming, vectorizing
with open('raw/Bundestag.csv', mode="r", encoding="cp1250") as fi:
    with open("processed/Bundestag_cleaned.csv",mode="w", encoding="utf-8") as fo:                
        reader = csv.reader(fi)
        next(reader) # skips header
        fieldnames = ['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'raw', 'no_int_raw', 'stems', 'no_int_stems', 'n_words_raw', 'n_words_noint']
        writer = csv.DictWriter(fo, lineterminator='\n', fieldnames = fieldnames)
        writer.writeheader() # define header
        i = 0
        
        for row in reader:
            if row[7]!='TRUE':
                
                
                # output indicating progress
                bar = str('\t\t[' + '='*int((i)/ (n/30)) + ' '*(30-int((i) / (n)/30)) + ']   ' + str((i)) + '/' + str(n))
                stdout.write('%s\r' % bar)
                stdout.flush()
                
                
                # get rid of interpellations in brackets in german data
                noint=re.sub("[\(].*?[\)]", " ", row[9])
                
                #remove punctuation
                row[9]=re.sub("[\.\,\?\/\!\-\[\]\(\)\;\:\'\"]", " ", row[9])
                noint=re.sub("[\.\,\?\/\!\-\[\]\(\)\;\:\'\"]", " ", noint)
                
                
                # stem
                stems_f = ''
                for w in nltk.word_tokenize(row[9]):
                    stems_f = stems_f + stemmer.stem(w) + ' '
                stems_r = ''
                for w in nltk.word_tokenize(noint):
                    stems_r = stems_r + stemmer.stem(w) + ' '
                
                # remove spaces for unstemmed
                row[9]=row[9].strip()
                noint=noint.strip()
                
                length_raw=len(nltk.word_tokenize(row[9]))
                length_noint=len(nltk.word_tokenize(noint))
                
                
                # write to csv with headers
                writer.writerow({'date':         row[1], 
                                 'agenda':       row[2],
                                 'id':           row[3],
                                 'speaker':      row[4],
                                 'party':        row[5],
                                 'partyfacts':   row[6], 
                                 'raw':          row[9], 
                                 'no_int_raw':   noint,
                                  'stems':        stems_f,
                                  'no_int_stems': stems_r,
                                 'n_words_raw':     length_raw,
                                 'n_words_noint':   length_noint
                                 })
                i += 1
                
print(f'\nFinished processing {i} German speeches')
