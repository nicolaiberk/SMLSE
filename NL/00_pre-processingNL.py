# -*- coding: utf-8 -*-
"""
Measuring Rhetorical Similarity with Supervised Machine Learning

Pre-processing Dutch Tweede Kamer Speeches

@author: Nicolai Berk
"""

# Setup
import csv
import datetime
import nltk
from sys import stdout
import os
import re

csv.field_size_limit(100000000)


n = 1050000
skip = 0
length=0
print('Processing Dutch speeches...')

with open('raw/TweedeKamer.csv', mode="r", encoding="cp1252") as fi:
    with open("processed/TweedeKamer_cleaned.csv",mode="w", encoding="utf-8") as fo:                
        reader = csv.reader(fi, delimiter=';')
        next(reader) # skips header
        fieldnames = ['date', 'id', 'party', 'partyfacts', 'speaker', 'agenda', 'raw', 'n_words']
        writer = csv.DictWriter(fo, lineterminator='\n', fieldnames = fieldnames)
        writer.writeheader() # define header
        i = 0
        
        for row in reader:
            if row[7]!='TRUE':
                
                
                # output indicating progress
                bar = str('\t\t[' + '='*int((i)/ (n/30)) + ' '*(30-int((i) / (n/30))) + ']   ' + str((i)) + '/' + str(n))
                stdout.write('%s\r' % bar)
                stdout.flush()
                
                #remove punctuation
                row[9]=re.sub("[\.\,\?\/\!\-\[\]\(\)\;\:\'\"]", " ", row[9])
                
                # remove spaces for unstemmed
                row[9]=row[9].strip()
                length=len(nltk.word_tokenize(row[9]))
                
                
                # write to csv with headers
                writer.writerow({'date':         row[1], 
                                 'agenda':       row[2],
                                 'id':           row[3],
                                 'speaker':      row[4],
                                 'party':        row[5],
                                 'partyfacts':   row[6], 
                                 'raw':          row[9], 
                                 'n_words':      length
                                 })
                i += 1
print(f'Finished processing {i} Dutch speeches')
