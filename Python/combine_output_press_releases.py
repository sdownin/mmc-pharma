# -*- coding: utf-8 -*-
"""
Created on Wed May 22 00:29:51 2019

@author: T430
"""
import os, pymongo, re
import pandas as pd
import numpy as np
from textstat import gunning_fog, text_standard ## concensus of RIs

work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))

## directories 
data_dir = os.path.join(work_dir, 'medtrack_data')
news_links_dir = os.path.join(data_dir, 'news_subcategories')
out_dir = os.path.join(work_dir, 'medtrack_data\\COMBINED\\20190521\\press_releases')
out_file = 'medtrack_press_release'

## FUNCTIONS
def fix_NaN(x):
    if isinstance(x, str) and x.lower() == 'nan':
        return None
    elif pd.isnull(x) and x != 0:
        return None
    else:
        return x


## Database Credentials
MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
MONGO_DATABASE = 'medtrack'
MONGODB_COLLECTION  = 'news_firm_separate'
#MONGO_PORT = 27017
MONGODB_COLLECTION_NEW  = 'press_release'

## database connection
client = pymongo.MongoClient(MONGO_URI)
collection = client[MONGO_DATABASE][MONGODB_COLLECTION]
collection_new = client[MONGO_DATABASE][MONGODB_COLLECTION_NEW]

## LOAD DATA FROM MONGODB
df = pd.DataFrame([ x for x in collection.find({}) ])

## replace news_subcategory NaN-->None
for col in df.columns:
    df[col] = df[col].apply(lambda x: fix_NaN(x))

##  duplicate links
mlink = df[['_id','link']].groupby('link').filter(lambda x: len(x) > 1)

## subset rows:
## either duplicate links with is_firm_source==1, or not duplicate link
idx_sr = (~df.link.isin(mlink.link) | ((df.is_firm_source > 0) & ~pd.isnull(df.news_subcategory)))
idx = idx_sr[idx_sr.values == True].index
df2 = df.iloc[idx,:].copy()

###DEBUG
#cnt2 = df2.link.value_counts()
#z = df2.loc[df2.link == 'http://dlx.medtrack.com//UI/Shared/Lookup/LUNews.aspx?NewsID=480102',:].copy()
#z.to_csv('TEST_DUPLICATES.csv',index_label='index')

## Add pharagraph and word numbers

## COLUMN NAME MAP
col_map = {
    '_id': 'ID',
    'release_date': 'DATE',
    'news_headline': 'HEADLINE',
    #'article': 'FULLTEXT',
    'article_text': 'BODYTEXT',
    'about_firm_1': 'ABOUTFIRM1',
    'about_firm_2': 'ABOUTFIRM2',
    'about_firm_3': 'ABOUTFIRM3',
    'about_firm_4': 'ABOUTFIRM4',
    'about_firm_5': 'ABOUTFIRM5',
    #'about_firm_6': 'ABOUTFIRM6',
    'source': 'SRC',
    'source_2': 'SRC2',
    'is_firm_source': 'ISFIRMSRC',
    'firm': 'FIRMID',
    'company_name': 'CONAME',
    'geography': 'COUNTRY',
    'indication': 'INDICATION',
    'news_category': 'CATEGORY',
    'news_subcategory': 'SUBCAT',
    'product_name': 'PRODNAME',
    'development_phase': 'DEVPHASE',
    'link': 'LINK',
    #'source_3': 'SRC3',
    #'source_4': 'SRC4',
    'about_other_1': 'ABOUTOTR1',
    'about_other_2': 'ABOUTOTR2',
    'about_other_3': 'ABOUTOTR3',
    'about_other_4': 'ABOUTOTR4',
    'about_other_5': 'ABOUTOTR5',
    'about_other_6': 'ABOUTOTR6',
    'about_other_7': 'ABOUTOTR7',
    'about_other_8': 'ABOUTOTR8',
    'about_other_9': 'ABOUTOTR9'#,
    #'about_other_10': 'ABOUTOTR10',
    #'about_other_11': 'ABOUTOTR11',
    #'about_other_12': 'ABOUTOTR12'
}

## CREATE NEW DATAFRAME SELECTING COLUMNS
col_select = [key for key in col_map.keys()]
df3 = df2[col_select].copy()

## MAP NAMES
df3.columns = [col_map[key] for key in col_map.keys()]


##================================================
## Readability Indices (complexity measure)
##------------------------------------------------
## strip returns  new lines chars
text_stripped = df3.BODYTEXT.apply(lambda x: x.replace('\r\n\r\n', ' ') if isinstance(x,str) else x)
## Gunning Fog Index
df3['RIGFOG'] = text_stripped.apply(lambda x: gunning_fog(x) if isinstance(x, str) else None)
## Consensus of Readability Indices;  @see https://pypi.org/project/textstat/
##   mode of: flesch_kincaid_grade()
##            flesch_reading_ease()
##            smog_index()
##            coleman_liau_index()
##            automated_readability_index()
##            dale_chall_readability_score()
##            linsear_write_formula()
##            gunning_fog()
df3['RICONSENSUS'] = text_stripped.apply(lambda x: text_standard(x, float_output=True) if isinstance(x, str) else None)


##================================================
##  Output
##------------------------------------------------
## CREATE NEW MONGODB COLLECTION
collection_new.insert_many([row.to_dict() for i,row in df3.iterrows()])

## Write CSV
csv_file = os.path.join(out_dir, '%s.csv' % out_file)
df3.to_csv(csv_file, index=False)

## Write Excel
xlsx_file = os.path.join(out_dir, '%s.xlsx' % out_file)
df3.to_excel(xlsx_file, index=False)















