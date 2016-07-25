# Kaggle script

#!/usr/bin/env python

#  Author: Angela Chapman
#  Date: 8/6/2014
#
#  This file contains code to accompany the Kaggle tutorial
#  "Deep learning goes to the movies".  The code in this file
#  is for Part 1 of the tutorial on Natural Language Processing.
#
# *************************************** #

import os
from sklearn.feature_extraction.text import CountVectorizer
# from sklearn.ensemble import RandomForestClassifier
from KaggleWord2VecUtility import KaggleWord2VecUtility
import pandas as pd
import numpy as np
import pickle


def preprocess_and_pickle(train_data_name, test_data_name):
  train = pd.read_csv(os.path.join(os.path.dirname(__file__), 'data', train_data_name), header=0, delimiter="\t", quoting=3)
  test = pd.read_csv(os.path.join(os.path.dirname(__file__), 'data', test_data_name), header=0, delimiter="\t", quoting=3 )

  print('The first review is: \n')
  print(train["review"][0])

  # raw_input("Press Enter to continue...")

  # print 'Download text data sets. If you already have NLTK datasets downloaded, just close the Python download window...'
  # nltk.download()  # Download text data sets, including stop words

  # Initialize an empty list to hold the clean reviews
  clean_train_reviews = []

  # Loop over each review; create an index i that goes from 0 to the length
  # of the movie review list

  print("Cleaning and parsing the training set movie reviews...\n")
  for i in range( 0, len(train["review"])):
    clean_train_reviews.append(" ".join(KaggleWord2VecUtility.review_to_wordlist(train["review"][i], True)))


  # ****** Create a bag of words from the training set
  #
  print("Creating the bag of words...\n")


  # Initialize the "CountVectorizer" object, which is scikit-learn's
  # bag of words tool.
  vectorizer = CountVectorizer(analyzer = "word",   
                           tokenizer = None,    
                           preprocessor = None,
                           stop_words = None,
                           max_features = 5000)

  # fit_transform() does two functions: First, it fits the model
  # and learns the vocabulary; second, it transforms our training data
  # into feature vectors. The input to fit_transform should be a list of
  # strings.
  train_data_features = vectorizer.fit_transform(clean_train_reviews)

  # Numpy arrays are easy to work with, so convert the result to an
  # array
  train_data_features = train_data_features.toarray()

  # Create an empty list and append the clean reviews one by one
  clean_test_reviews = []

  print("Cleaning and parsing the test set movie reviews...\n")
  for i in range(0,len(test["review"])):
    clean_test_reviews.append(" ".join(KaggleWord2VecUtility.review_to_wordlist(test["review"][i], True)))

  # Get a bag of words for the test set, and convert to a numpy array
  test_data_features = vectorizer.transform(clean_test_reviews)
  test_data_features = test_data_features.toarray()
  
  pickle_train_name = "pickle_" + train_data_name
  pickle_test_name = "pickle_" + test_data_name
  pickle.dump(train_data_features, open(pickle_train_name, "wb"))
  pickle.dump(test_data_features, open(pickle_test_name, "wb"))

  return pickle_train_name, pickle_test_name



def load_pickled_data(pickle_train_name, pickle_test_name):
  with open(pickle_test_name, 'rb') as ptest:
    test_data_features = pickle.load(ptest)
  with open(pickle_train_name, 'rb') as ptest:
    train_data_features = pickle.load(ptest) 
  return test_data_features, train_data_features



if __name__ == '__main__':
  
    train_data_name = 'labeledTrainData.tsv'
    test_data_name = 'testData.tsv'
    pickle_train_name = "pickle_" + train_data_name
    pickle_test_name = "pickle_" + test_data_name

    try:
      # try loading pickled data
      test_data_features, train_data_features = load_pickled_data(pickle_test_name, pickle_train_name)
    except:
      # instead preprocess data
      pickle_train_name, pickle_test_name = preprocess_and_pickle(train_data_name, test_data_name)
      test_data_features, train_data_features = load_pickled_data( pickle_train_name = pickle_train_name, pickle_test_name = pickle_test_name)

    print("sample of data: ", train_data_features[:20], test_data_features[:20])
    
    #
    # I will use regularized logistic regression instead of random forest...
    #

    # Copy the results to a pandas dataframe with an "id" column and
    # a "sentiment" column
    #output = pd.DataFrame( data={"id":test["id"], "sentiment":result} )

    # Use pandas to write the comma-separated output file
    #output.to_csv(os.path.join(os.path.dirname(__file__), 'data', 'Bag_of_Words_model.csv'), index=False, quoting=3)
    # print("Wrote results to Bag_of_Words_model.csv")
      
