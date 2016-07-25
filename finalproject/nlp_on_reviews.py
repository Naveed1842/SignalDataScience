# Process the amazon review text
import csv
import nltk
import re
from nltk.corpus import stopwords
import string
import bs4 as BeautifulSoup

dirty_data = []
with open("/home/hadoop/data_node/Data Analysis/review500kdata.txt", "r") as dirty_file:
    dirty_data = dirty_file.read()

# dirty_data[ind2][ind] = nltk.pos_tag(dirty_data[ind2][ind]) 

def review_to_words(raw_review, stops):
    # Function to convert a raw review to a string of words
    # The input is a single string (a raw movie review), and 
    # the output is a single string (a preprocessed movie review)
    #
    # 1. Remove non-letters        
    letters_only = re.sub("[^a-zA-Z]", " ", review_text) 
    #
    # 2. Convert to lower case, split into individual words
    words = letters_only.lower().split()                             
    #
    # 3. Remove stop words
    meaningful_words = [w for w in words if not w in stops]   
    #
    # 4. Join the words back into one string separated by space, 
    # and return the result.
    return( " ".join( meaningful_words ))


#  In Python, searching a set is much faster than searching
#  a list, so convert the stop words to a set
stops = set(stopwords.words("english"))                  

reviews = []
for raw_review in dirty_data:
    reviews.append(review_to_words(raw_review, stops))

print(reviews[0])

with open("cleaned_reviews.csv", "w") as cleaner_file:
    wr = csv.writer(cleaner_file, delimiter=',')
    wr.writerows(cleaner_text)  

