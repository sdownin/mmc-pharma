# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""
import os, pymongo
import pandas as pd
## if called from outside project dir
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))
from medtrack_article_parser import MedtrackArticle, get_user_agent

## directories 
data_dir = os.path.join(work_dir, 'medtrack_data')
news_links_dir = os.path.join(data_dir, 'news_links')

## Database Credentials
MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
MONGO_DATABASE = 'medtrack'
MONGODB_COLLECTION  = 'news_firm'
#MONGO_PORT = 27017


def run():
    """ Main run script
    """
    ## database connection
    client = pymongo.MongoClient(MONGO_URI)
    collection = client[MONGO_DATABASE][MONGODB_COLLECTION]
    USER_AGENT = get_user_agent()
    
    ## read in collection contents if any
    df = pd.DataFrame([ x for x in collection.find({}) ])
    
    for index, row in df.iterrows():
        if index % 100 == 0:
            print(' %s' % index)
        #print(type(row.article_strip))
        if 'article_strip' in row.index and isinstance(row.article_strip, str) and len(row.article_strip):
            continue
        article = MedtrackArticle(row.link, collection, row.to_dict(), USER_AGENT)
        article.parse()
        article.update_by_id({
            'article': article.item['article'],
            'article_strip': row.article
        })


if __name__ == '__main__':
    run()



