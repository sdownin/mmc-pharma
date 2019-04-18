# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 12:34:08 2019

@author: T430
"""

import os
import pandas as pd
from argparse import ArgumentParser
from manual_prcrawler import ManualPRCrawler


def run():
    """ Main web crawler run function
    """
    work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
    data_dir = os.path.join(work_dir, 'analysis_data')

    # ## add logs directory
    # log_dir = os.path.join(work_dir,'logs')
    # if not os.path.exists(log_dir):
    #     os.makedirs(log_dir)

    # ## logging
    # configure_logging(install_root_handler=False)
    # logging.basicConfig(
    #     filename=os.path.join(log_dir,'log_%s.txt' % round(timestamp())),
    #     format='%(levelname)s: %(message)s',
    #     level=logging.INFO
    # )
    
    ## parse arguments
    par = ArgumentParser(description="Run Manual Industry Press Release Crawler")
    par.add_argument('-f','--files', type=str, help="The data files to process (comma separated)") 
    par.add_argument('-n','--names', type=str, help="The firm names to crawl (comma separated)") 
    args = par.parse_args()
    files = args.files.split(',') if args.files is not None else []
    names = args.names.split(',') if args.names is not None else []
    
    ## if no files specified, run all files in data dir
    if not files:
        files = os.listdir(data_dir)
    print('files to crawl:')
    print(files)
    
    ## run crawlers for each data file
    for file in files:
        
        ## check data file
        if file not in os.listdir(data_dir):
            print('skipping missing file %s' % file)
            next
        else:
            print('processing file: %s' % file)
        
        ## load data
        df = pd.read_csv(os.path.join(data_dir, file), na_values=[""])
           
        ## run web crawlers per domain in data file
        for index, row in df.iterrows():
            ## check firm name
            if names and row.name not in names:
                next
            ## dynamic content: temporarily skip
            if int(row.pdf):
                print(' skipping %s for dynamic pdf content' % row.name)
                next
            ## runner crawl spider 
            print(' running firm %s' % row.name)
            clr = ManualPRCrawler(row.to_dict(), data_dir)
            clr.fetch_article_urls()
            clr.fetch_articles()
            print(clr.article_urls)
            

if __name__ == '__main__':
    run()
