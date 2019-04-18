# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""
import os, pymongo
import pandas as pd
from argparse import ArgumentParser
## if called from outside project dir
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))
from medtrack_article_parser import MedtrackArticle, fix_header, get_user_agent


def run():
    """ Main run script
    """
    ## directories 
    data_dir = os.path.join(work_dir, 'medtrack_data')
    news_links_dir = os.path.join(data_dir, 'news_links')
    
    ## Database Credentials
    MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
    MONGO_DATABASE = 'medtrack'
    MONGODB_COLLECTION  = 'news'
    #MONGO_PORT = 27017

    ## database connection
    client = pymongo.MongoClient(MONGO_URI)
    collection = client[MONGO_DATABASE][MONGODB_COLLECTION]
    USER_AGENT = get_user_agent()
    
    ## parse arguments
    par = ArgumentParser(description="Fetch and Parse Medtrack News Articles")
    par.add_argument('-f','--file', type=str, help="File names or substring of name to process") 
    args = par.parse_args()
    file = args.file
    
    ##====================================================
    ## GET NEWS BODY FOR EACH ARTICLE
    ##----------------------------------------------------
    dir_files = [x for x in os.listdir(news_links_dir) if '.xls' in x]
    ##
    if file is not None:
        run_files = [x for x in dir_files if file in x]
    else:
        run_files = dir_files
    
    for i, file_i in enumerate(run_files):
        print(' running file: %s' % file_i)
        file_i_path = os.path.join(news_links_dir, file_i)
        file_i_parsed_path = os.path.join(news_links_dir, 'PARSED_%s' % file_i)
        df = pd.read_excel(file_i_path, skiprows=11)
        df.columns = [fix_header(x) for x in df.columns]
        
        ## add column to check if article is parsed
        if 'is_parsed' not in df.columns:
            df['is_parsed'] = df.link.apply(lambda _: None)
            
        ## process each row to parse articles
        for index, row in df.iterrows(): 
            article = MedtrackArticle(row.link, collection, row.to_dict(), USER_AGENT)
            article.parse()
            if article.item.keys():
                df.loc[index,'is_parsed'] = 1
                article.save()
            if index % 20 == 0:
                print('  %s: %s' % (index, row.news_headline[:40]))
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




