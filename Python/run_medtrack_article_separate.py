# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""
import os, pymongo, re
import pandas as pd
from bson.objectid import ObjectId
## if called from outside project dir
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))

## directories 
data_dir = os.path.join(work_dir, 'medtrack_data')
news_links_dir = os.path.join(data_dir, 'news_links')

## Database Credentials
MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
MONGO_DATABASE = 'medtrack'
MONGODB_COLLECTION  = 'news_firm'
MONGODB_COLLECTION_NEW  = 'news_firm_separate'
#MONGO_PORT = 27017

## map Firm ID to firm name search pattern
firms = {
    'pfizer': ['pfizer'],
    'gsk': ['glaxo','kline','gsk'],
    'merck': ['merck','plough'],  ## 2009 merck merged with schering-plough
    'jnj': ['johnson','jnj'],
    'novartis': ['novartis'],
    'roche': ['roche'],
    'sanofi': ['sanofi'],
    'astrazeneca': ['astrazeneca'],
    'bms': ['bristol','squibb','bms'],
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
    print('processing article separation')
    ## database connection
    client = pymongo.MongoClient(MONGO_URI)
    collection = client[MONGO_DATABASE][MONGODB_COLLECTION]
    collection_new = client[MONGO_DATABASE][MONGODB_COLLECTION_NEW]
    
    ## read in collection contents if any
    df = pd.DataFrame([ x for x in collection.find({}) ])
    
    ## Process all rows to new collection
    for index, row in df.iterrows():
        
        if index % 500 == 0:
            print(' %s' % index)
        #print(type(row.article_strip))
        if 'article_source' in row.index and isinstance(row.article_source, str) and len(row.article_source):
            continue
        
        ## press release record as a dictionary
        item = row.to_dict()
        
        ## fix parsed null types that cannot save in MongoDB (pd.NaT, ...)
        for key in item.keys():
            if pd.isnull(item[key]):
                item[key] = None
        
        ## Patch missing firm IDs (if multiple Company Name listed)
        firm_set = set()
        for firm_id in firms.keys():
            for firm_pattern in firms[firm_id]:
                if firm_pattern in item['company_name'].lower():
                    firm_set.update([firm_id])
        item['firm'] = '|'.join(firm_set)
        
        ## Source
        source_pattern = '\\r\\n\\r\\nSource:\s.*(?!\\r|\\n)'
        sources = re.findall(source_pattern, item['article'])
        is_firm_source = 0
        for i, source_i in enumerate(sources):
            source_i_str = re.sub('\\.$','', source_i.replace('\r\n\r\nSource:', '') ).strip()  ## remove final period and strip whitespace
            if i == 0:
                key = 'source'
                for firm_id in item['firm'].split('|'):
                    if firm_id in firms.keys():
                        for firm_pattern in firms[firm_id]:
                            if firm_pattern in source_i_str.lower():
                                is_firm_source = 1
                item['is_firm_source'] = is_firm_source
            else:
                key = 'source_%s' % (i+1)
            item[key] = source_i_str 
        
        ## About sections
        item['about_firm_1'] = None ## set default firm about in case not in press release
        about_pattern = '\\r\\n\\r\\nAbout.*\\r\\n\\r\\n.*(?!\\r|\\n)'
        abouts = re.findall(about_pattern, item['article'])
        about_firm_cnt = 0
        about_other_cnt = 0
        for i, about_i in enumerate(abouts):
            about_i_text = re.sub('\\r\\n\\r\\nAbout.*\\r\\n\\r\\n','', about_i)
            about_i_firm = re.sub('(\\r\\n\\r\\nAbout|\\r\\n\\r\\n)','',about_i.replace(about_i_text,'')).strip()
            ##--------------
            is_focal_firm = False
            for firm_id in item['firm'].split('|'):
                if firm_id in firms.keys():
                    for firm_pattern in firms[firm_id]:
                        if firm_pattern in about_i_firm.lower():
                            is_focal_firm = True
            ##--------------
            if is_focal_firm:
                key = 'about_firm_%s' % (about_firm_cnt + 1)
                about_firm_cnt += 1
            else:
                key = 'about_other_%s' % (about_other_cnt + 1)
                about_other_cnt += 1
            item[key] = '|'.join([about_i_firm,about_i_text])
        
        ## Article without the Abouts and Sources
        ending_pattern = '(\\r\\n\\r\\nAbout.*\\r\\n\\r\\n.*(?!\\r|\\n)|\\r\\n\\r\\nSource:\s.*(?!\\r|\\n))'
        article_text = re.sub(ending_pattern,'', item['article'])
        item['article_text'] = article_text.strip()

        ## Save to new collection
        if not collection_new.find_one({'_id':ObjectId(item['_id'])}):
            _id = collection_new.insert_one(item)
            if not _id or not _id.inserted_id:
                print(' failed to insert',item['_id'])
        
        # ## ****DEBUG***
        # if index > 200:
        #     break


if __name__ == '__main__':
    run()



