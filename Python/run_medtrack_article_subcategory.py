# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""
import os, pymongo, re
import pandas as pd
## if called from outside project dir
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))
from medtrack_article_parser import fix_header


def parse_data_filename(x):
    """ data filename format
        ex: 'Medtrack_CE-News_Apr2019_mylan-NasdaqGS-MYL.xls'
        Medtrack_CE-News_{month}{year}_{firm}-{exchange}-{symbol}.xls
    """
    base = 'Medtrack_CE-News_'
    x = x.replace(base, '')
    ext = x.split('.')[-1]
    x = x.split('.%s' % ext)[0]
    parts = x.split('_')
    date = parts[0]
    symbols = parts[1].split('-')
    firm = symbols[0]
    exchange = symbols[1]
    symbol = symbols[2]
    return dict(firm=firm,symbol=symbol,exchange=exchange,ext=ext,
                year = int(re.sub('[a-zA-Z]+','',date)),
                month = re.sub('[0-9]+','',date))
    

def run():
    """ Main run script
    """
    ## directories 
    data_dir = os.path.join(work_dir, 'medtrack_data')
    news_links_dir = os.path.join(data_dir, 'news_subcategories')
    
    ## Database Credentials
    MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
    MONGO_DATABASE = 'medtrack'
    MONGODB_COLLECTION  = 'news_firm'
    #MONGO_PORT = 27017

    ## database connection
    client = pymongo.MongoClient(MONGO_URI)
    collection = client[MONGO_DATABASE][MONGODB_COLLECTION]
    #USER_AGENT = get_user_agent()

    ###====================================================
    ### 2. ADD FIRM NEWS SUBCATEGORY TO NEWS ARTICLE BODY
    ###----------------------------------------------------
    run_files = [x for x in os.listdir(news_links_dir) if '.xls' in x]
    
    for i, file_i in enumerate(run_files):
        ## parse filename
        file_data = parse_data_filename(file_i)
        print(' running file: %s' % file_i)
        file_i_path = os.path.join(news_links_dir, file_i)
        file_i_parsed_path = os.path.join(news_links_dir, '_PARSED_%s' % file_i)
        df = pd.read_excel(file_i_path, skiprows=11)
        df.columns = [fix_header(x) for x in df.columns]
        
        ## add column to check if article is parsed
        if 'is_parsed' not in df.columns:
            df['is_parsed'] = df[df.columns[0]].apply(lambda _: None)
            
        ## process each row to parse articles
        for index, row in df.iterrows(): 
            q = {'news_headline':row.news_headline, 
                 'firm':file_data['firm']}
            field = {'news_subcategory':row.news_subcategory,
                     'sc_release_date': row.release_date,
                     'sc_news_category': row.news_category,
                     'sc_product_name': row.product_name,
                     'sc_trial_phase': row.trial_phase,
                     'sc_indication': row.indication,
                     'sc_industry': row.industry,
                     'sc_geography': row.geography,
                     'sc_technology_name': row.technology_name}
            result = collection.update_one(q, {"$set": field})
            if result.modified_count:
                df.loc[index,'is_parsed'] = 1
            if index % 1000 == 0 or index == (df.shape[0] - 1):
                print('  %s: %s' % (index, row.news_headline[:50]))
                df.to_excel(file_i_parsed_path, index=False)
                

if __name__ == '__main__':
    run()



###====================================================
### 2. ADD FIRM NEWS SUBCATEGORY TO NEWS ARTICLE BODY
###----------------------------------------------------
#
#
#field = {'news_subcategory':'TESTING_TESTING_TESTING_123'}
#article.update(field)




