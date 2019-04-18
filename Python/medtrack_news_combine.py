# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""

import os, re, requests, pymongo
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd
#import sys
#from time import time, sleep
#from threading import Thread
#from multiprocessing import Queue


## Database Credentials
MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
MONGO_PORT = 27017
MONGO_DATABASE = 'medtrack'
MONGODB_COLLECTION  = 'news'

## directories 
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
data_dir = os.path.join(work_dir, 'medtrack_data')
news_links_dir = os.path.join(data_dir, 'news_links')

## functions
def fix_header(x):
    return re.sub('\s+', '_', x.strip().lower())

def clean_text(x):
    return re.sub('[\\r\\n]', '', x.strip())

def get_user_agent(x=None):
    """ select user agent string or randomly choose from list
    """
    USER_AGENT_LIST = [
       #Chrome
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36',
        'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
        'Mozilla/5.0 (Windows NT 5.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
        'Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
        'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36',
        'Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36',
        'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36',
        'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36',
        #Firefox
        'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1)',
        'Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko',
        'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)',
        'Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko',
        'Mozilla/5.0 (Windows NT 6.2; WOW64; Trident/7.0; rv:11.0) like Gecko',
        'Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko',
        'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.0; Trident/5.0)',
        'Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; rv:11.0) like Gecko',
        'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)',
        'Mozilla/5.0 (Windows NT 6.1; Win64; x64; Trident/7.0; rv:11.0) like Gecko',
        'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)',
        'Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)',
        'Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)'
    ]
    n = len(USER_AGENT_LIST)
    index = np.random.choice(range(0,n), 1)[0] if x is None else x
    return USER_AGENT_LIST[index]


##==========================================
##  MAIN ARTICLE PARSER CLASS
##------------------------------------------
class MedtrackArticle(object):
    
    def __init__(self, url, collection, item={}, user_agent=None):
        self.url = url
        self.collection = collection
        self.item = item
        self.headers = {
            'User-Agent': user_agent if user_agent is not None else get_user_agent()
            #,'From': 'acannella'  # This is another valid field
        }
        
    def parse(self):
        """ Parse article text and metadata from html page
        returned from given 
        """
        try:
            r = requests.get(self.url, headers=self.headers)
            soup = BeautifulSoup(r.text)
            # titles = soup.select('h1, h2, h3')
            # self.item['title'] = clean_text(titles[0].text)
            trs = soup.select('tr.odd')
            for i,tr in enumerate(trs):
                trsoup = BeautifulSoup('<html>%s</html>' % tr)
                tds = trsoup.select('td')
                key = fix_header(clean_text(tds[0].text) )
                if key == '':
                    key = 'article'
                val = clean_text(tds[1].text) if len(tds) > 1 else ''
                self.item[key] = val
            print('OK')
        except requests.exceptions.HTTPError as errh:
            print ("Http Error:",errh)
        except requests.exceptions.ConnectionError as errc:
            print ("Error Connecting:",errc)
        except requests.exceptions.Timeout as errt:
            print ("Timeout Error:",errt)
        except requests.exceptions.RequestException as err:
            print ("OOps: Something Else",err)

    def save(self, collection=None):
        collection = self.collection if collection is None else collection
        _id = collection.insert_one(self.item)
        self.item['_id'] = _id.inserted_id
        if '_id' in self.item.keys():
            print(' inserted Mongodb _id: %s' % self.item['_id'])

    def update(self, field, collection=None):
        if not isinstance(field, (dict)):
            raise ValueError(' `field` must be dict type.')
        collection = self.collection if collection is None else collection
        collection.update_one(self.item, {"$set": field})


##====================================================
## DB CON
##----------------------------------------------------
client = pymongo.MongoClient(MONGO_URI)
collection = client[MONGO_DATABASE][MONGODB_COLLECTION]
USER_AGENT = get_user_agent()

##====================================================
## 1. GET NEWS BODY FOR EACH ARTICLE
##----------------------------------------------------

files = [x for x in os.listdir(news_links_dir) if '.xls' in x]

file_i = files[0]

df = pd.read_excel(os.path.join(news_links_dir, file_i), skiprows=11)
df.columns = [fix_header(x) for x in df.columns]

for index, row in df.iterrows(): 
    print(' %s' % row.news_headline[:40])
    article = MedtrackArticle(row.link, collection, row.to_dict(), USER_AGENT)
    article.parse()
    if article.item.keys():
        article.save()
        
###====================================================
### 2. ADD FIRM NEWS SUBCATEGORY TO NEWS ARTICLE BODY
###----------------------------------------------------
#
#
#field = {'news_subcategory':'TESTING_TESTING_TESTING_123'}
#article.update(field)






#class MedtrackThread(object):
#    
#    def __init__(self, files, collection):
#        self.files = files
#        self.collection = collection
#
#   def _getStatus(self, url):
#        try:
#            response = requests.get(url)
#        except Exception as e:
#            print(e)
#            return "error", url
#        length = response.headers['content-length'] if 'content-length' in response.headers else 0
#        return url, response.status_code, length
#
#    def _handleResults(self):
#        while True:
#            url = self.q.get()
#            url, status, length = self._getStatus(url)
#            resultDict = dict(url=url,status=status,length=length,time=ar.now())
#            self.li.extend([resultDict])
#
#    def run(self, calls=100):
#        self.concurrent = len(self.files)
#        self.q = Queue(self.concurrent)
#        t0 = time()
#        ## start threads
#        for i in range(self.concurrent):
#            self.t = Thread(target=self._handleResults)
#            self.t.daemon = True
#            self.t.start()
#        ## make calls
#        for url in self.urls:
#            try:
#                for _ in range(self.calls):
#                    self.q.put(url.strip())
#                    #q.join_thread()
#            except KeyboardInterrupt:
#                sys.exit(1)
#        ## wait for queue to finish
#        while not self.q.empty():
#            sleep(1)
#        ## results list to dataframe
#        keepTrying = True
#        while keepTrying:
#            try:
#                self.df = pd.DataFrame(self.li)
#                keepTrying = False
#            except Exception as e:
#                print(e)
#                sleep(1)


