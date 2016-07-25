# OkCupid Analysis Part 2

library("softImpute")
library("readr")
library("psych")

demographics_df = data.frame(read_csv("okcupid_demographics.csv"))
questionKey_df = data.frame(read_csv("okcupid_questionKey.csv"))

train_df = data.frame(read_csv("train_melted_df.csv"))
test_df = data.frame(read_csv("test_melted_df.csv"))



fake_uid = max(train_df$uid2) + 1
fake_qid = max(train_df$qid2) + 1

fake_question = data.frame(matrix(data = NA, nrow=length(unique(train_df$uid2)), ncol=3))
fake_question[,1] = unique(train_df$uid2)
fake_question[,2] = rep(fake_qid)
fake_question[,3] = mean(train_df$value) + rnorm(nrow(fake_question), mean = 0, sd = 0.01)
names(fake_question) = c("uid2", "qid2", "value")
train_df_full = rbind(train_df[3:5], fake_question)


fake_user = matrix(data = NA, nrow = unique(nrow(train_df)), ncol = 3, dimnames = list(NULL, colnames(df)))


fake_user = data.frame(matrix(data = NA, nrow=length(unique(train_df$qid2)), ncol=3))
fake_user[,1] = rep(fake_uid)
fake_user[,2] = unique(train_df$qid2)
fake_user[,3] = mean(train_df$value) + rnorm(nrow(fake_user), mean = 0, sd = 0.01)
names(fake_user) = names(train_df_full)
train_df_full = rbind(train_df_full, fake_user)

# Fake full user and fake full question added
nrow(train_df) + nrow(fake_user) + nrow(fake_question)
# Generate sparse matrix with softImpute's Incomplete()
train_df_full$qid3 = as.numeric(as.factor(train_df_full$qid2))
sparse_train = Incomplete(train_df_full$uid2, train_df_full$qid3, train_df_full$value)

sparse_scale = biScale(sparse_train, maxit = 5, trace = TRUE)

lam0 = lambda0(sparse_scale)

lam_vec = seq(from = log(lam0), to = log(1), length.out = 20)

lam_vec_exp = exp(lam_vec)
lam_vec_exp

results = data.frame(lambda = lam_vec_exp, rank=NA, rmse =NA)

fits = list(rep(NA, length(lam_vec_exp)))
count =1


fits[1] = list(softImpute(sparse_scale, rank.max = 30, lambda = lam_vec_exp[1], maxit = 1000))

round_and_count = function(x){
  count = 0
  for(i in 1:length(x)){
    if(round(x[i], 4) != 0) count = count + 1
  }
  return(count)
}

test_df$qid3 = as.numeric(as.factor(test_df$qid2))

rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}

for(lam in lam_vec_exp){
  if(count == 1){
    #fits[count] = list(softImpute(sparse_scale, rank.max = 30, lambda = lam, maxit = 1000))
  } else {
    fits[count] = list(softImpute(sparse_scale, rank.max = 30, lambda = lam, maxit = 1000, warm.start = fits[[count-1]]))
  }
  results[count, "rank"] = round_and_count(unlist(fits[[count]]['d']))
  results[count, "rmse"] = rmse(impute(fits[[count]],                               test_df$uid2,                               test_df$qid3),                           test_df$value)
  print(c(count, lam, results[count, "rmse"], results[count, "rank"]))
  count = count +1
}


best_svd = fits[[min(results$rmse)]]

mae = function(x,y){
  mae = sum(abs(x-y))/(length(x))
}


missing_cov = cov(as.matrix(sparse_train), use = "pairwise.complete.obs", method = "pearson")
?cov
?fa
missing_fa = fa(missing_cov, nfactors = 7, covar = TRUE)





