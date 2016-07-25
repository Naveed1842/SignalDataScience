# OkCupid data analysis

library("readr")


melted_df = data.frame(read_csv("okcupid_melted2.csv"))


#melted_df$uid2 = as.numeric(as.factor(melted_df$uid))
#melted_df$qid2 = as.numeric(as.factor(melted_df$qid))
#write_csv(melted_df, "okcupid_melted2.csv")
#melted_df$uid = NULL
#melted_df$qid = NULL

train_index = sample(nrow(melted_df), nrow(melted_df)*0.8, replace = FALSE)
test_index = setdiff(1:nrow(melted_df), train_index)

user_ids = unique(melted_df$uid2)
q_ids = unique(melted_df$qid2)

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)) stop('x and y must have the same length')
  return(sqrt(mean((x-y)^2)))
}

colmean_RMSE = rmse(melted_df$value, rep(mean(melted_df$value), nrow(melted_df)))

train_value_scaled = scale(train_dfvalue)

test_value_scaled = scale(test_df$value, center = attr(train_value_scaled,"scaled:center"), scale = attr(train_value_scaled,"scaled:scale"))

test_df$test_value_scaled = test_value_scaled

train_df = cbind(melted_df[train_index,], train_value_scaled)
write_csv(train_df, "train_melted_df.csv")

test_df = cbind(melted_df[test_index,], test_value_scaled)
write_csv(test_df, "test_melted_df.csv")
rm(melted_df)
rm(test_value_scaled)
rm(train_value_scaled)


