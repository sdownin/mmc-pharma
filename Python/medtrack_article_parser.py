# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""

import re, requests
from bs4 import BeautifulSoup
import numpy as np
#import sys
#from time import time, sleep
#from threading import Thread
#from multiprocessing import Queue

## functions
def fix_header(x):
    x = re.sub('-', '', x.strip().lower())
    return re.sub('\s', '_', x)

def clean_text(x):
    return re.sub('[\\r\\n]+', '', x.strip())


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
        if 'is_parsed' in item.keys():
            del item['is_parsed']
        self.item = item
        self.headers = {
            'User-Agent': user_agent if user_agent is not None else get_user_agent()
            #,'From': 'acannella'  # This is another valid field
        }
        
    def parse(self, verbose=False):
        """ Parse article text and metadata from html page
        returned from given URL
        """
        try:
            r = requests.get(self.url, headers=self.headers)
            soup = BeautifulSoup(r.text, 'html.parser')
            # titles = soup.select('h1, h2, h3')
            # self.item['title'] = clean_text(titles[0].text)
            trs = soup.select('tr.odd')
            for i,tr in enumerate(trs):
                if i < (len(trs)-1):  ## metadata before the final row
                    trsoup = BeautifulSoup('<html>%s</html>' % tr, 'html.parser')
                    tds = trsoup.select('td')
                    key = fix_header(clean_text(tds[0].text))
                    val = tds[1].text.strip() if len(tds) > 1 else ''
                else:   ## article text in the final row
                    key = 'article'
                    val =  tr.text.strip()  ## clean_text(tr.text)  # don't remove \r,\n yet
                self.item[key] = val
            if verbose:
                print(" OK")
        except requests.exceptions.HTTPError as errh:
            print(" Http Error:",errh)
        except requests.exceptions.ConnectionError as errc:
            print(" Error Connecting:",errc)
        except requests.exceptions.Timeout as errt:
            print(" Timeout Error:",errt)
        except requests.exceptions.RequestException as err:
            print(" OOps: Something Else",err)

    def save(self, collection=None, verbose=False):
        collection = self.collection if collection is None else collection
        if not collection.find_one(self.item):
            _id = collection.insert_one(self.item)
            self.item['_id'] = _id.inserted_id
            if verbose and '_id' in self.item.keys():
                print(' inserted Mongodb _id: %s' % str(self.item['_id']))

    def update(self, field, collection=None):
        if not isinstance(field, (dict)):
            raise ValueError(' `field` must be dict type.')
        collection = self.collection if collection is None else collection
        collection.update_one(self.item, {"$set": field})


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


