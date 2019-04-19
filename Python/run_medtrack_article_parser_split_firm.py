# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""
import os, pymongo
import pandas as pd
#from argparse import ArgumentParser
## if called from outside project dir
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))
from medtrack_article_parser import MedtrackArticle, fix_header

firms = {
    'pfizer': ['pfizer'],
    'gsk': ['glaxo'],
    'merck': ['merck','plough'],  ## 2009 merck merged with schering-plough
    'jnj': ['johnson'],
    'novartis': ['novartis'],
    'roche': ['roche'],
    'sanofi': ['sanofi'],
    'astrazeneca': ['astrazeneca'],
    'bms': ['bristol','squibb'],
    'abbott': ['abbott'],
    'teva': ['teva'],
    'bayer': ['bayer'],
    'lilly': ['lilly'],
    'novonordisk': ['nordisk'],
    'gilead': ['gilead'],
    'amgen': ['amgen'],
    'genzyme': ['genzyme'],
    'biogen': ['biogen'],
    'abbvie': ['abbvie'],
    'mylan': ['mylan'],
}



def run():
    """ Main run script
    """
    ## directories 
    data_dir = os.path.join(work_dir, 'medtrack_data')
    news_links_dir = os.path.join(data_dir, 'news_subcategories')
    
    ## Database Credentials
    MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
    MONGO_DATABASE = 'medtrack'
    MONGODB_COLLECTION_FROM  = 'news'
    MONGODB_COLLECTION_TO    = 'news_firm'
    #MONGO_PORT = 27017

    ## database connection
    client = pymongo.MongoClient(MONGO_URI)
    collection_from = client[MONGO_DATABASE][MONGODB_COLLECTION_FROM]
    collection_to = client[MONGO_DATABASE][MONGODB_COLLECTION_TO]

    ###====================================================
    ### 
    ###----------------------------------------------------
    df = pd.DataFrame([x for x in collection_from.find({})])
    df['news_id'] = df._id.apply(lambda x: str(x))
    del df['_id']
    df['firm'] = None
    
    data_to = []
    for index, row in df.iterrows():
        
        if index % 1000 == 0:
            print(' %s' % index)
        for firm_key in firms.keys():
            
            for firm_str in firms[firm_key]:
                row_i = row.copy()
                if firm_str in row_i.company_name.lower():
                    row_i['firm'] = firm_key
                    data_to.append(row_i.to_dict())

    print( 'inserting %s records to collection `%s`' % (len(data_to), MONGODB_COLLECTION_TO))
    collection_to.insert_many(data_to)
    
    

if __name__ == '__main__':
    run()


#
#fx = []
#for index, row in df.iterrows():
#    fx.extend(row.company_name.lower().split(', '))
#
#fxs = pd.Series(fx)
#fxsvc = fxs.value_counts()

###====================================================
### 2. ADD FIRM NEWS SUBCATEGORY TO NEWS ARTICLE BODY
###----------------------------------------------------
#
#
#field = {'news_subcategory':'TESTING_TESTING_TESTING_123'}
#article.update(field)




