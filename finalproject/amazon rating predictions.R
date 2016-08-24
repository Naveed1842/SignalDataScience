# Amazon reviews predictive analysis

# Load libraries

# Part 1: convert data to wide format
# Load long data
path = "/Users/nathan/Documents/Stats/SignalDataScience/finalproject/datafiles/"
df = read.csv(paste0(path,'reviewsummary.csv'))
df['X'] = NULL

# Convert to wide format - one dummy column set per review
# In each dummy column set:
# reviewerScore, overall, vader (sentiment analysis)
# so twenty reviews should become 60 columns

# target variable is avg_rating, which I'll rename true_rating

unmade=TRUE
asin_count = 1
for(asin in unique(df$asin)){
  if(unmade==TRUE){
    df_wide = matrix(data = NA, nrow = length(unique(df$asin)), ncol=(length(unique(df[df['asin']==asin,'reviewerID'])) * 3 + 2))
    unmade=FALSE
  }
  user_count = 1
  df_wide[asin_count, user_count] = asin
  print(df[df['asin']==asin,'avg_review'][1])
  df_wide[asin_count, user_count+1] = df[df['asin']==asin,'avg_rating'][1]
  for (reviewerID in unique(df[df['asin']==asin,'reviewerID'])){
    
    rating = df[df['asin']==asin & df['reviewerID']==reviewerID,'overall']
    sentiment = df[df['asin']==asin & df['reviewerID']==reviewerID,'vader']
    reviewer_score = df[df['asin']==asin & df['reviewerID']==reviewerID,'SUM.reviewerScore.']
    df_wide[asin_count, user_count+2] = rating
    df_wide[asin_count, user_count+3] = sentiment
    df_wide[asin_count, user_count+4] = reviewer_score
    user_count = user_count + 3
  }
  asin_count = asin_count + 1
}

df_wide = data.frame(df_wide)
colnames(df_wide)[1] = "asin"
colnames(df_wide)[2] = "true_rating"
user_count = 1
for(user_num in seq(3,ncol(df_wide),3)){
  prefix = paste0("review", as.character(user_count), "_")
  colnames(df_wide)[user_num] = paste0(prefix, "rating")
  print(paste0(prefix, "rating"))
  colnames(df_wide)[user_num+1] = paste0(prefix, "sentiment")
  colnames(df_wide)[user_num+2] = paste0(prefix, "reviewer_score")
  user_count = user_count + 1
}
print(colnames(df_wide))

write.csv(df_wide, paste0(path, "wide_summary.csv"), row.names = FALSE)

# Part 2: make a model!
path = "/Users/nathan/Documents/Stats/SignalDataScience/finalproject/datafiles/"
df_wide = read.csv(paste0(path, "wide_summary.csv"), header = TRUE)



