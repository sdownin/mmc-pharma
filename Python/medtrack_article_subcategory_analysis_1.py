# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 01:21:28 2019

@author: T430
"""
import os, pymongo, re
import pandas as pd
from argparse import ArgumentParser
## if called from outside project dir
work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
os.chdir(os.path.join(work_dir, 'Python'))
from medtrack_article_parser import MedtrackArticle, fix_header
import datetime as dt

import gensim
from gensim.utils import simple_preprocess
from gensim.parsing.preprocessing import STOPWORDS
from nltk.stem import WordNetLemmatizer, SnowballStemmer
from nltk.stem.porter import *
import numpy as np
np.random.seed(2018)
import nltk
nltk.download('wordnet')


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
### Data from mongodb
###----------------------------------------------------
df = pd.DataFrame([ x for x in collection.find({}) ])

def extract_year(x):
    if isinstance(x, dt.datetime):
        return x.year
    if x is None or len(x) < 2 or isinstance(x, (np.float)):
        return x
    year_str = re.findall('\d{4}',x)[0]
    # print(year_str)
    if isinstance(year_str, (str)) and len(year_str) == 4:
        return int(year_str)

print(df.shape)
df['year'] = df.release_date.apply(lambda x: extract_year(x))
df['first_category'] = df.news_category.apply(lambda x: x.split(',')[0].lower().strip())
df['first_subcategory'] = df.news_subcategory.apply(lambda x: x.split(',')[0][:12].lower().strip() if isinstance(x, str) else None)
df = df.loc[(df.year > 2004) & (df.year <= 2018),].copy()

print('articles with subcategory: %s' % 
      df.loc[~pd.isna(df.news_subcategory), ].shape[0])

nsct = df[['year','news_subcategory']].groupby('year').count()
nsct.plot()

nsctf = df[['year','firm']].groupby(['year','firm']).size().unstack(fill_value=0)
nsctf.plot(figsize=(10,7))

nsct2 = df[['year','first_category']].groupby(['year','first_category']).size().unstack(fill_value=0)
nsct2.plot(figsize=(10,7))

nsct3 = df[['year','first_subcategory']].groupby(['year','first_subcategory']).size().unstack(fill_value=0)
nsct3.plot(figsize=(10,10), logy=True)

nsct3 = df[['year','news_subcategory']].groupby(['year','news_subcategory']).size()
nsct3.plot(kind='line')




def lemmatize_stemming(text):
    return stemmer.stem(WordNetLemmatizer().lemmatize(text, pos='v'))

def preprocess(text):
    result = []
    for token in gensim.utils.simple_preprocess(text):
        if token not in gensim.parsing.preprocessing.STOPWORDS and len(token) > 3:
            result.append(lemmatize_stemming(token))
    return result

doc_sample = documents[documents['index'] == 4310].values[0][0]
print('original document: ')
words = []
for word in doc_sample.split(' '):
    words.append(word)
print(words)
print('\n\n tokenized and lemmatized document: ')
print(preprocess(doc_sample))

processed_docs = documents['headline_text'].map(preprocess)
processed_docs[:10]

dictionary = gensim.corpora.Dictionary(processed_docs)
count = 0
for k, v in dictionary.iteritems():
    print(k, v)
    count += 1
    if count > 10:
        break
    
dictionary.filter_extremes(no_below=15, no_above=0.5, keep_n=100000)   
    
bow_corpus = [dictionary.doc2bow(doc) for doc in processed_docs]
bow_corpus[4310]


from gensim import corpora, models
tfidf = models.TfidfModel(bow_corpus)
corpus_tfidf = tfidf[bow_corpus]
from pprint import pprint
for doc in corpus_tfidf:
    pprint(doc)
    break


lda_model = gensim.models.LdaMulticore(bow_corpus, num_topics=10, id2word=dictionary, passes=2, workers=2)


for idx, topic in lda_model.print_topics(-1):
    print('Topic: {} \nWords: {}'.format(idx, topic))
    
    
    

lda_model_tfidf = gensim.models.LdaMulticore(corpus_tfidf, num_topics=10, id2word=dictionary, passes=2, workers=4)
for idx, topic in lda_model_tfidf.print_topics(-1):
    print('Topic: {} Word: {}'.format(idx, topic))
    
    
    
for index, score in sorted(lda_model[bow_corpus[4310]], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model.print_topic(index, 10)))
    


for index, score in sorted(lda_model_tfidf[bow_corpus[4310]], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model_tfidf.print_topic(index, 10)))
    
    
unseen_document = 'How a Pentagon deal became an identity crisis for Google'
bow_vector = dictionary.doc2bow(preprocess(unseen_document))
for index, score in sorted(lda_model[bow_vector], key=lambda tup: -1*tup[1]):
    print("Score: {}\t Topic: {}".format(score, lda_model.print_topic(index, 5)))

