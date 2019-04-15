# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 12:34:08 2019

@author: T430
"""

import os
import pandas as pd
import pymongo

work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
data_dir = os.path.join(work_dir, 'analysis_data')

# Database Credentials
URI = 'localhost' #'mongodb://127.0.0.1'
PORT = 27017
DATABASE = 'prcrawler'
COLLECTION  = 'items'

## init mongodb connection
client = pymongo.MongoClient(URI)
collection = client[DATABASE][COLLECTION]

## COLLECTION counts
collection.count_documents({'article':{'$ne':None}})  ## has article: 7342
collection.count_documents({})  ## total:  18377

## load COLLECTION into dataframe where `article` exists
cursor = collection.find({'article':{'$ne':None}}) ## total:  18377
df = pd.DataFrame([x for x in cursor])
print(df)

## close client
client.close()

## clean articles
df['article'] = df.article.apply(lambda x: ' '.join(x.split()))

## check
ch = df[['spider','title','article']].drop_duplicates().copy()
ch['title'] = df.title.apply(lambda x: x.trim()[:30])
ch['article'] = df.article.apply(lambda x: x.trim()[:80])
print(ch)

## save to csv
ch.to_csv(os.path.join(data_dir,'check_articles_1.csv'), index=False)
