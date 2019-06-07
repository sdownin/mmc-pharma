# -*- coding: utf-8 -*-
"""
Created on Thu Jun  6 00:18:32 2019

@author: T430
"""

import os, pymongo, re
import pandas as pd
from bson.objectid import ObjectId
## if called from outside project dir
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))

## Database Credentials
MONGO_URI = 'localhost' #'mongodb://127.0.0.1'
MONGO_DATABASE = 'medtrack'
MONGODB_COLLECTION  = 'press_release'

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

## database connection
client = pymongo.MongoClient(MONGO_URI)
collection = client[MONGO_DATABASE][MONGODB_COLLECTION]

## read in collection contents if any
df = pd.DataFrame([ x for x in collection.find({}) ])


def get_match_list(pattern, x, range_len=200):
    """Return list of matches from `pattern` in string `x`,
       including +/- `range_len` chars in the matched str
    """
    matches = []
    for result in re.finditer(pattern, x):
        b1,b2 = result.span()  ## main match range
        a1,a2 = (max(b1-range_len,0), max(b1,0)) ## pre-match range
        c1,c2 = (min(b2,len(x)-1), min(b2+range_len,len(x)-1)) ## post-match range
        x_cat = '%s{{%s}}%s' % (x[a1:a2], x[b1:b2], x[c1:c2])
        x_sub = re.sub('\r\n\s{,2}\r\n', ' ', x_cat)
        matches.append(x_sub)
    return matches

def get_series_match_list(pattern, df, series, range_len=200):
    """Return list of get_match_list() matches 
       for pd.Series of strings
    """
    matches = []
    for i, row in df.iterrows():
        x = row[series]
        if isinstance(x, str):
            row_matches = get_match_list(pattern, x, range_len)
            if row_matches:
                key = '%s|%s' % (row.FIRMID, str(row.ID)[-8:])
                matches.append({key:'......'.join(row_matches)})
    return matches


##===========================================
## GROWTH
##-------------------------------------------
test_grow = """
an aging population that is
leading to increasing demand…rapidly growing market;
the increased demand should continue.
What demand increase do you expect?
""".replace('\n',' ')

##PATTERNS
#grow_ptn_c = '(increas(e|ed|ing).{,10}demand|demand.{,10}increas)'
grow_ptn_c = '(increas(?!ed)(e|es|ing).{,10}demand|demand.{,10}increas)'
## Individualist GROWTH
#grow_ptn_i = '(gr(ew|ow|owing|owth).{,10}sale|sale.{,10}gr(ew|ow|owing|owth))'
grow_ptn_i = '(poised|primed|ready|positioned) (to|for).{,10}grow(th|ing)'

## MATCHES
grow_c = get_series_match_list(grow_ptn_c, df, 'BODYTEXT')
grow_i = get_series_match_list(grow_ptn_i, df, 'BODYTEXT')


##===========================================
## R&D
##-------------------------------------------

##PATTERNS
#grow_ptn_c = '(increas(e|ed|ing).{,10}demand|demand.{,10}increas)'
rnd_ptn_c = 'research.{,15}consorti(a|um)'
rnd_ptn_c = '((manag|consist|commit|maintain|reduc|shrink|limit).{,15}pipelin|pipelin.{,15}(manage|consist|commit|maintain|reduc|shrink|limit))'
## Individualist GROWTH
#grow_ptn_i = '(gr(ew|ow|owing|owth).{,10}sale|sale.{,10}gr(ew|ow|owing|owth))'
rnd_ptn_i = '((accelerat|grow|continu).{,15}pipelin|pipelin.{,15}(accelerat|grow|continu))'

## MATCHES
rnd_c = get_series_match_list(rnd_ptn_c, df, 'BODYTEXT')
rnd_i = get_series_match_list(rnd_ptn_i, df, 'BODYTEXT')





##===========================================
## ENTRY BARRIERS
##-------------------------------------------

##PATTERNS
## collectivistic
bar_ptn_c = '(( end|loss|cliff|wall|termina).{,10}patent|patent.{,10}(cliff|wall| end|termina|loss))'
bar_ptn_c = 'en(d|ding|ds|ded).{,10}exclusiv'
bar_ptn_c = 'lobb(y|ying|ies|ied)'
bar_ptn_c = '((alliance|agreement).{,15}exclusi|exclusi.{,15}(alliance|agreement))'
bar_ptn_c = '((alliance|agreement).{,15}exclusi|exclusi.{,15}(alliance|agreement))'
bar_ptn_c = 'cross-licens'
## Individualist 
bar_ptn_i = '((end|cliff|wall|termina)e.{,15}exclus|exclus.{,15}(accelerat|grow|continu))'
bar_ptn_i = '((accelerat|grow|continu).{,15}pipelin|pipelin.{,15}(accelerat|grow|continu))'


## MATCHES
bar_c = get_series_match_list(bar_ptn_c, df, 'BODYTEXT')
bar_i = get_series_match_list(bar_ptn_i, df, 'BODYTEXT')


















