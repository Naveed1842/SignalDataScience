# Sven Chilton and Nathan Helm-Burger
# Signal Data Science Cohort 3

# Set the working directory
setwd('~/GitHub/signal-work/rscripts/')

# Load the appropriate packages
library(pROC)

# Extract the names of the training and testing emails
# Add the path of each email relative to the working directory
train_emails = list.files('../data/CSDMC2010_SPAM/TRAINING/')
train_emails = paste0('../data/CSDMC2010_SPAM/TRAINING/',train_emails)
test_emails  = list.files('../data/CSDMC2010_SPAM/TESTING/')
test_emails  = paste0('../data/CSDMC2010_SPAM/TESTING/',test_emails)

## Combine the train and test email vectors into a single vector
#emails = c(train_emails, test_emails)

# Scan each email in the vectors to get the data
train_content = vector('list',length(train_emails))
for (i in 1:length(train_emails)) {
  train_content[[i]] = scan(file=train_emails[i], 
                            what=character(), 
                            allowEscapes=FALSE)
}
test_content = vector('list',length(test_emails))
for (i in 1:length(test_emails)) {
  test_content[[i]] = scan(file=test_emails[i], 
                           what=character(), 
                           allowEscapes=FALSE)
}

# Since the emails start at 0, set the names of the 
# train and test content lists equal to 0:(length(list)-1)
names(train_content) = 0:(length(train_content)-1)
names(test_content)  = 0:(length(test_content)-1)


# We got warning messages, so let's make sure that scan() 
# actually read in what it was supposed to
for (i in 1:length(train_content)) {
  print(paste0('i = ',i,', length(train_content[[i]]) = ',
               length(train_content[[i]])))
}
for (i in 1:length(test_content)) {
  print(paste0('i = ',i,', length(test_content[[i]]) = ',
               length(test_content[[i]])))
}

length(train_content[[3521]])
length(train_content[['3520']])

# Load the data indicating whether a given email in the 
# training set is ham (1) or spam (0)
ham_key = read.csv('../data/CSDMC2010_SPAM/SPAMTrain.label', 
                   sep = ' ', header = FALSE)
colnames(ham_key) = c('is_ham','eml_file')
rownames(ham_key) = 0:(nrow(ham_key)-1)

# Find all unique words in the training and test content
all_words = unique(c(unlist(train_content), unlist(test_content)))
all_test_words = unique(unlist(test_content))

# Make train and test subsets of the training data
split_list = function(full_list, train_frac=0.5) {
  len = length(full_list)
  train_size = floor(train_frac*len)
  shuff_rows = sample(1:len, len, replace = FALSE)
  train_ind  = head(shuff_rows, train_size)
  test_ind   = tail(shuff_rows, -train_size) 
  l = vector("list", 2)
  names(l) = c('train','test')
  l[['train']] = full_list[train_ind]
  l[['test']]  = full_list[test_ind]
  return(l)
}


split_train = split_list(train_content, train_frac = 0.8)
sub_train   = split_train[['train']]
sub_test    = split_train[['test']]

# Find the unique words in the training and test subsets 
# of the original training data
all_sub_train_words = unique(unlist(sub_train))
all_sub_test_words  = unique(unlist(sub_test))


# Populate a df associated with the sub_train data
# Each row corresponds to an email, each column corresponds to 
# a word.  Each entry is 1 if the given email contains a given 
# word and 0 otherwise.
sub_train_mat = matrix(nrow=length(sub_train),
                    ncol=length(all_sub_train_words))
for (row in 1:nrow(sub_train_mat)) {
  sub_train_mat[row,] = 
    as.numeric(all_sub_train_words %in% sub_train[[row]])
}
#names(sub_train)[1348] = "NA"
sub_train_df = data.frame(sub_train_mat)
colnames(sub_train_df) = all_sub_train_words
rownames(sub_train_df) = names(sub_train)

# Populate a df associated with the sub_test data
# Each row corresponds to an email, each column corresponds to 
# a word.  Each entry is 1 if the given email contains a given 
# word and 0 otherwise.
sub_test_mat = matrix(nrow=length(sub_test),
                      ncol=length(all_sub_test_words))
for (row in 1:nrow(sub_test_mat)) {
  sub_test_mat[row,] = 
    as.numeric(all_sub_test_words %in% sub_test[[row]])
}
sub_test_df = data.frame(sub_test_mat)
names(sub_test_df) = all_sub_test_words

# Add is_ham columns to sub_train_df and sub_test_df
sub_train_df = 
  cbind(sub_train_df, is_ham = ham_key[rownames(sub_train_df),'is_ham'])
sub_test_df = 
  cbind(sub_test_df, is_ham = ham_key[rownames(sub_test_df),'is_ham'])
#colnames(sub_train_df)[length(colnames(sub_train_df))] = 'is_ham'
#colnames(sub_test_df)[length(colnames(sub_test_df))] = 'is_ham'


# Calculate P(spam | wi) for each word in the training set
# FOR NOW (go back and fix this) remove the pesky NA from 
# sub_train_df
sub_train_df = na.omit(sub_train_df)
num_sub_train_words = length(all_sub_train_words)
Pspam_for_words = rep(NA, num_sub_train_words)
for (column in 1:num_sub_train_words){
  print(paste0('Starting column ',column,' out of ',num_sub_train_words))
  word = colnames(sub_train_df)[column]
  spam_set = sub_train_df[sub_train_df$is_ham == 0, column]
  ham_set  = sub_train_df[sub_train_df$is_ham == 1, column]
  spam_occurances = sum(spam_set)
  ham_occurances  = sum(ham_set)
  print(paste0('spam_occurances = ',spam_occurances))
  print(paste0('ham_occurances = ', ham_occurances))
  if(spam_occurances == 0) {
    Pspam_for_words[column] = 0.001
  } else if (ham_occurances == 0){
    Pspam_for_words[column] = 0.999
  } else {
    Pspam_for_words[column] = 
      spam_occurances / (spam_occurances + ham_occurances)
  }
}
names(Pspam_for_words) = all_sub_train_words

# To save on computational time, run further calculations with 
# only the 20000 most common words in our sub-training set, 
# with commonality defined as the number of emails in the sub-
# training set in which a given word occurs.
most_common_words = 
  head(names(sort(colSums(sub_train_df), decreasing=TRUE)),20000)


# For each email in the train subset, calculate probability of
# given email being spam based on the words in it
# This took foreeeeeeeeeeever to run, so we must have done 
# something wrong
email_spam_probability = as.numeric(rep(NA, nrow(sub_train_df)))

for(email_row in 1:nrow(sub_train_df)){
  print(paste0('Starting email_row ',email_row))
  words_in_row = 
    sub_train[[email_row]][sub_train[[email_row]] %in% most_common_words]
  # We erroneously made '' a word, which has an NA 
  # probability of being spam
  # Thus, let's eliminate it from words_in_row
  words_in_row = words_in_row[words_in_row != '']
  num_words_in_row = length(words_in_row)
  right_side = vector('numeric',num_words_in_row)
  for(word_num in 1:num_words_in_row){
    right_side[word_num] = 
      log(1-Pspam_for_words[words_in_row[word_num]]) - 
      log(Pspam_for_words[words_in_row[word_num]])
  }
  spam_prob = 1/(exp(sum(right_side)) + 1)
  if (spam_prob != 0) print(paste0('spam_prob = ',spam_prob))
  #print(paste0('spam_prob of email_row ',email_row,' = ',spam_prob))
  email_spam_probability[email_row] = spam_prob
}
names(email_spam_probability) = rownames(sub_train_df)

# For each email in the test subset, calculate probability of
# given email being spam based on the words in it
# If any word in a test email is not present in the training 
# set words vector, P(Spam|word) = 0.4 
most_common_test_words = 
  head(names(sort(colSums(sub_test_df), decreasing=TRUE)),20000)

sub_test_trimmed = 
  lapply(sub_test, function(email) email[email %in% most_common_test_words])

test_spam_probability = as.numeric(rep(NA, nrow(sub_test_df)))

for (email_row in 1:nrow(sub_test_df)) {
  print(paste0('Starting email_row ',email_row))
  words_in_row = sub_test_trimmed[[email_row]]
  num_words_in_row = length(words_in_row)
  right_side = vector('numeric',num_words_in_row)
  for (word_num in 1:num_words_in_row) {
    word = words_in_row[word_num]
    if (word %in% most_common_words) Pspam = Pspam_for_words[word]
    else Pspam = 0.4
    right_side[word_num] = log(1-Pspam) - log(Pspam)
  }
  spam_prob = 1/(exp(sum(right_side)) + 1)
  if (spam_prob != 0) print(paste0('spam_prob = ',spam_prob))
  #print(paste0('spam_prob of email_row ',email_row,' = ',spam_prob))
  test_spam_probability[email_row] = spam_prob
}
names(test_spam_probability) = rownames(sub_test_df)

# Calculate the performance measures of both the sub-training 
# and sub-test sets

# Get logical vectors of actual and predicted positives and 
# negatives for both the sub-training and sub-testing sets
sub_train_actual_spam    = sub_train_df$is_ham == 0
sub_train_actual_ham     = sub_train_df$is_ham == 1
sub_train_predicted_spam = email_spam_probability >= 0.5
sub_train_predicted_ham  = email_spam_probability <  0.5

sub_test_actual_spam     = sub_test_df$is_ham == 0
sub_test_actual_ham      = sub_test_df$is_ham == 1
sub_test_predicted_spam  = test_spam_probability >= 0.5
sub_test_predicted_ham   = test_spam_probability <  0.5

# Compute the fractions of 
# (true, false) x (pos, neg) x (sub_train, sub_test)
num_sub_train = length(sub_train)
num_sub_test  = length(sub_test)
sub_train_tp = 
  sum(sub_train_actual_spam & sub_train_predicted_spam)/num_sub_train
sub_train_tn = 
  sum(sub_train_actual_ham & sub_train_predicted_ham)/num_sub_train
sub_train_fp = 
  sum(sub_train_actual_ham & sub_train_predicted_spam)/num_sub_train
sub_train_fn = 
  sum(sub_train_actual_spam & sub_train_predicted_ham)/num_sub_train

sub_test_tp = 
  sum(sub_test_actual_spam & sub_test_predicted_spam)/num_sub_test
sub_test_tn = 
  sum(sub_test_actual_ham & sub_test_predicted_ham)/num_sub_test
sub_test_fp = 
  sum(sub_test_actual_ham & sub_test_predicted_spam)/num_sub_test
sub_test_fn = 
  sum(sub_test_actual_spam & sub_test_predicted_ham)/num_sub_test

# Draw the ROC curves.  Heh.  
roc(as.numeric(!sub_train_df$is_ham), email_spam_probability, plot = TRUE)
roc(as.numeric(!sub_test_df$is_ham),  test_spam_probability, plot = TRUE)





