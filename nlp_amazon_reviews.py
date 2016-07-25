
# coding: utf-8

# In[12]:

import pandas as pd


# In[2]:

# Process the amazon review text
import csv
import nltk
import re
from nltk.corpus import stopwords
import string
import bs4 as BeautifulSoup



# In[47]:

import numpy as np
import itertools
np.set_printoptions(precision=9)
import math
import timeit
import gzip

path = '/home/hadoop/data_node/Data Analysis/'
text_destination = '/home/hadoop/data_node/Data Analysis/tmp/chunk' 
global debug 
debug = False

def parse(path):
    g = gzip.open(path + 'item_dedup.json.gz', 'rb')
    for l in g:
        yield eval(l)


def Load_Review_Chunk(path, destination, chunksize):
    start_time = timeit.default_timer()
    dfDict = {}
    i = 0
    chunk = 1
    total_chunks = 82680000/chunksize
    for d in parse(path):
        dfDict[i] = d
        i += 1
        if i > chunksize * chunk:
            # save via function
            review_to_csv(dfDict, destination + str(chunk) + ".csv")
            time_e = int(timeit.default_timer() - start_time)
            time_rem = (time_e * total_chunks / chunk) - time_e
            print("Chunk # ", chunk, " saved. Time elapsed: ", time_e/60, " minutess. Time remaining: ", time_rem/60, end='\r')
            dfDict = {}
            chunk += 1
            if debug == True and chunk == 3: break
    # last save via function if needed
    if len(dfDict)>0:
        review_to_csv(dfDict, destination + str(chunk) + ".csv")
    print("Last Chunk # ", chunk, "Total time elapsed: ", int(timeit.default_timer() - start_time) / 60, " minutes", end='\r')
    return

def review_to_csv(dfDict, destination):
    # cols: reviewer_id, item_id (asin), helpful
    df = pd.DataFrame.from_dict(dfDict, orient='index')
    if debug == True:
        print(df.head())
    # col_list = ['reviewerID', 'asin', 'reviewText','overall', 'unixReviewTime']
    df.drop(['helpful', 'reviewerName', 'summary', 'reviewTime'], axis=1, inplace = True)
    df.to_csv(destination, header = True, sep='\n', index=False)
    return
  

 
Load_Review_Chunk(path, text_destination, 5000)



# In[ ]:




# In[3]:

dirty_data = []
with open("/home/hadoop/data_node/Data Analysis/review500kdata.txt", "r") as dirty_file:
    dirty_data = dirty_file.read().split("\n")


# In[35]:

with open("/home/hadoop/data_node/Data Analysis/cleaned_reviews.csv", "w") as cleaner_file:
    wr = csv.writer(cleaner_file, delimiter=',')


# In[36]:

def review_to_words(raw_review, stops):
    # Function to convert a raw review to a string of words
    # The input is a single string (a raw movie review), and 
    # the output is a single string (a preprocessed movie review)
    #
    # 1. Remove non-letters        
    letters_only = re.sub("[^a-zA-Z]", " ", raw_review) 
    #
    # 2. Convert to lower case, split into individual words
    words = letters_only.lower().split()                             
    #
    # 3. Remove stop words
    meaningful_words = [w for w in words if (not w in stops and len(w) > 2)]   
    #
    # 4. Join the words back into one string separated by space, 
    # and return the result.
    return( " ".join( meaningful_words ))


#  In Python, searching a set is much faster than searching
#  a list, so convert the stop words to a set
stops = set(stopwords.words("english"))                  

reviews = []
count = 1
for raw_review in dirty_data:
    reviews.append(review_to_words(raw_review, stops))
    if count % 10 == 0:
        print("current_review: ")
        print(reviews[2])
        print("Reviews processed: ", count)
        df = pd.DataFrame(reviews)
        df.to_csv("/home/hadoop/data_node/Data Analysis/cleaned_reviews.csv", mode = 'a', header = False, sep='\n', index=False)
        reviews = []
    count += 1
    # debugging only!
    if count == 100 and debug == True: break


# In[29]:

if debug == True:
    check_reviews = pd.read_csv("/home/hadoop/data_node/Data Analysis/cleaned_reviews.csv", sep='\n')
    print(check_reviews)


# In[33]:




# In[34]:




# In[ ]:



