# -*- coding: utf-8 -*-
"""
Created on Wed May 22 00:29:51 2019

@author: T430
"""
import os, pymongo, re
import pandas as pd

work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))

## directories 
data_dir = os.path.join(work_dir, 'medtrack_data')
news_links_dir = os.path.join(data_dir, 'news_subcategories')
out_dir = os.path.join(work_dir, 'medtrack_data\\COMBINED\\20190521\\press_releases')

## Database Credentials
MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
MONGO_DATABASE = 'medtrack'
MONGODB_COLLECTION  = 'news_firm'
#MONGO_PORT = 27017
MONGODB_COLLECTION_NEW  = 'press_release'

## database connection
client = pymongo.MongoClient(MONGO_URI)
collection = client[MONGO_DATABASE][MONGODB_COLLECTION]

collection_new = client[MONGO_DATABASE][MONGODB_COLLECTION_NEW]

## LOAD DATA FROM MONGODB
df = pd.DataFrame([ x for x in collection.find({}) ])

## COLUMN NAME MAP
col_map = {
    '_id': 'ID',
    'release_date': 'DATE',
    'news_headline': 'HEADLINE',
    'article': 'FULLTEXT',
    'company_name': 'CONAME',
    'firm': 'FIRMID',
    'development_phase': 'DEVPHASE',
    'geography': 'COUNTRY',
    'indication': 'INDICATION',
    'news_category': 'CATEGORY',
    'news_subcategory': 'SUBCAT',
    'product_name': 'PRODNAME',
    'link': 'LINK',
}

## CREATE NEW DATAFRAME SELECTING COLUMNS
col_select = [key for key in col_map.keys()]
df2 = df[col_select].copy()

## MAP NAMES
df2.columns = [col_map[key] for key in col_map.keys()]

## CREATE NEW MONGODB COLLECTION
collection_new.insert_many([row.to_dict() for i,row in df2.iterrows()])

