write.csv(marincountyzipcodes, "marinzip2.csv", col.names = FALSE, row.names = FALSE, eol = ",")
?write.csv
?scan
?list.files
files = list.files("")
corpus = list(rep(NA, length(test_files)))
path = "path"
for (i in 1:length(test_files)){
  corpus[i] = scan(file = paste0(path, files[i]), what = character(), allowEscapes = FALSE)
}

list_test = list(1,2,3,'g')
list_test[1] = "t"
list_test[[1]] = 5

all_words = unique(unlist(train_content))

df_matrix = matrix(nrows = length(train_files), ncol = length(all_words))
for(j in 1:length(train_files)){
  df_matrix[j,] = all_words %in% train_files[i]
}


unique(unlist(all_emails))
matrix( )
for(email in all_emails){
  matrix[] = list_of_all_words %in% email
}

# Calculate P(spam | wi) for each word in the training set

Pspam_for_words = vector("numeric",rep(NA, ncol(train_df)))
for (column in 1:ncol(train_df)){
  word = colnames(train_df)[column]
  spam_set = train_df[[!ham_rows,column]]
  ham_set = train_df[[ham_rows, column]]
  spam_occurances = sum(as.numeric(spam_set %in% word))
  ham_occurances = sum(as.numeric(ham_set %in% word))
  if(spam_occurances == 0) {
    Pspam_for_words[column] = 0.001
  } else if (ham_occurances == 0){
    Pspam_for_words[column] = 0.999
  } else {
    Pspam_for_words[column] = spam_occurances / (spam_occurances + ham_occurances)
  }
}


names(Pspam_for_words) = colnames(train_df)

# For each email in the train subset, calculate probability of
# given email being spam based on the words in it
email_spam_probability = vector("numeric", rep(NA, nrow(train_df)))
?vector
for(email_num in 1:nrow(test_df)){
  right_side = vector("numeric", rep(0, ncol(train_df)))
  for(word in 1:length(Pspam_for_words)){
    right_side[word] = log(1 - Pspam_for_words[word]) - log(Pspam_for_words[word])
  }
  email_spam_probability[email_num]= 1/(exp(sum(right_side)) + 1)
}

# Plot associated ROC curve, AUC, false positive rate, false negative rate


dummydf = data.frame(matrix(sample(2, 100, replace = TRUE)-1, ncol = 5))
View(dummydf)
colnames(dummydf) = c("a", "b", "c", "d", "e")

colnames(dummydf[1,dummydf[1,]==1])

goob = c(1,2,3,5)
names(goob) = c("a", "b", 1, 1)
goob
goob['b']




