# Amazon reviews predictive analysis

# Load libraries

# Part 1: convert data to wide format
# Load long data
path = "/Users/nathan/Documents/Stats/SignalDataScience/finalproject/datafiles/"
df = read.csv(paste0(path,'reviewsummary.csv'), stringsAsFactors = FALSE)
df['X'] = NULL
sum(is.na(df))
length(unique(df$asin))
# Convert to wide format - one dummy column set per review
# In each dummy column set:
# reviewerScore, overall, vader (sentiment analysis)
# so twenty reviews should become 60 columns

# target variable is avg_rating, which I'll rename true_rating
asin = df[90,'asin']
length(unique(df[df['asin']==asin,'reviewerID']))

unmade=TRUE
asin_count = 1
for(asin in unique(df$asin)){
  if(unmade==TRUE){
    df_wide = matrix(data = NA, 
                     nrow = length(unique(df$asin)), 
                     ncol=(length(unique(df[df['asin']==asin,'reviewerID'])) * 3 + 3))
    unmade=FALSE
  }
  user_count = 4
  df_wide[asin_count, 1] = asin
  # print(df[df['asin']==asin,'avg_rating'][1])
  df_wide[asin_count, 2] = df[df['asin']==asin,'avg_rating'][1]
  df_wide[asin_count, 3] = mean(df[df['asin']==asin,'overall'])
  for (reviewerID in unique(df[df['asin']==asin,'reviewerID'])){
    
    rating = df[df['asin']==asin & df['reviewerID']==reviewerID,'overall']
    sentiment = df[df['asin']==asin & df['reviewerID']==reviewerID,'vader']
    reviewer_score = df[df['asin']==asin & df['reviewerID']==reviewerID,'SUM.reviewerScore.']
    df_wide[asin_count, user_count] = rating
    df_wide[asin_count, user_count+1] = sentiment
    df_wide[asin_count, user_count+2] = reviewer_score
    user_count = user_count + 3
  }
  asin_count = asin_count + 1
  if(asin_count %% 100 == 0){
    print(paste0("Reviews processed: ", asin_count))
  }
}

df_wide = data.frame(df_wide)
print(paste0("Dimensions of wide dataframe:", dim(df_wide)))
colnames(df_wide)[1] = "asin"
colnames(df_wide)[2] = "true_rating"
colnames(df_wide)[3] = "rough_rating"
user_count = 1
for(user_num in seq(4,ncol(df_wide),3)){
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

##### Select Features and Target #######
library('caret')
library('ranger')
library('pROC')
library('ROCR')

split_data = function(df, test_fraction){
  df$grouping = sample(test_fraction, dim(df)[1], replace = TRUE)
  train = subset(df, df$grouping %% test_fraction != 0)
  test = subset(df, df$grouping %% test_fraction == 0)
  return(list(train = train, test = test))
}


model_comparisons = data.frame("model-metric"=NA, "performace_measure"=NA)
path = "/Users/nathan/Documents/Stats/SignalDataScience/finalproject/datafiles/"
df_wide = read.csv(paste0(path, "wide_summary.csv"), header = TRUE, stringsAsFactors = FALSE)
sum(is.na(df_wide))
split_df = split_data(df_wide, 5)
target = df_wide[['true_rating']]
target_factor_train = as.factor(as.numeric(as.numeric(as.character(split_df$train$true_rating))>=4))
target_factor_test = as.factor(as.numeric(as.numeric(as.character(split_df$test$true_rating))>=4))
summary(target)
summary(df_wide$rough_rating)
levels(target_factor_train) = c("under4", "over4")
levels(target_factor_test) = c("under4", "over4")
#rough_rating_factor = as.factor(as.numeric(df_wide$rough_rating>=4))
#levels(rough_rating_factor) = c("under4", "over4")
features_train = split_df$train[4:ncol(split_df$train)]
features_test = split_df$test[4:ncol(split_df$test)]

predictions = df_wide['rough_rating']
# Should both be zero
sum(is.na(target))
sum(is.na(features))

feat_targ = df_wide[c(2,4:ncol(df_wide))]
# Graphing the ROC curve for predicting target_factor by rough_rating

# with pROC package - plot.roc(predicted, data) two vectors: the continuous guess, and the true values
plot.roc(target_factor, df_wide$rough_rating)

# with ROCR package - 
plotROC <- function(truth, predictions, ...){
  # make predictions a matrix if vector
  if(is.null(ncol(predictions))){
    predictions = matrix(predictions)
  }
  pred <- prediction(predictions, matrix(truth, nrow = length(truth), ncol = ncol(predictions)))    
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  
  plot(perf, ...)
}
roc = function(new_prediction){
  predictions = cbind(predictions, new_prediction)
  plotROC(truth = as.numeric(target_factor), predictions = predictions)
}
plotROC(truth = as.numeric(target_factor), predictions = predictions, add = TRUE)

?plot.roc
?performance
######  Cumulative Mean Graph  ############
# Graphing the cumulative function of mean as it gets more accurate
library(reshape2)
cum_mean = function(v){
  # accepts vector of numeric inputs
  # returns a vector of the cum mean
  mean_list = c(rep(NA, length(v)))
  for(i in 1:length(v)){
    mean_list[i] = mean(v[1:i])
  }
  return(mean_list)
}
extract_cum_mean = function(df_wide){
  col_seq = seq(4, ncol(df_wide), 3)
  # print(col_seq)
  asin_vec = c()
  first_loop = TRUE
  df_matrix = matrix(data = NA, nrow = length(df_wide$asin), ncol = length(col_seq))
  for(asin in df_wide$asin){
    asin_vec = c(asin_vec, asin)
    vec = unlist(df_wide[df_wide['asin']==asin, col_seq])
    df_matrix[length(asin_vec),] = cum_mean(vec) - rep(as.numeric(as.character(df_wide[df_wide['asin']==asin, 'true_rating'])), length(vec))
  }
  df_cm = data.frame(df_matrix)
  df_cm = cbind(asin_vec, df_cm)
  colnames(df_cm) = c("asin", colnames(df_wide[col_seq]))
  return(df_cm)
}

products = 20
test_cm_df = df_wide[1:(products*3),]
test_cm_df = extract_cum_mean(test_cm_df)
colnames(test_cm_df)[2:ncol(test_cm_df)]= as.character(1:(ncol(test_cm_df)-1))

# Look for hot products (rising stars) by looking for a cluster of reviews chronologically close and also high in rating
# Get a baseline of twenty reviews at random, and predict the true rating from there
# that will confirm that the prediction algorithm works

# look at products who cross zero... go from lower to higher or visa versa
# eg. min is <0 and max is >0
# sort into overrated, underrated, switch

melt_cm_df = melt(test_cm_df, id='asin')
ggplot(melt_cm_df, aes(x=variable, y=value, colour=asin, group=asin)) + 
  geom_line() + 
  labs(x="Number of Reviews", y="Cumulative Mean") + 
  ylim(-5,5) +
  geom_hline(aes(yintercept=0))


#### n-fold cross-validation  #####
nfold_cv = function(df, n_folds, to_predict='target') {
  # Total number of rows in the df
  n_rows = nrow(df)
  # Find the number of rows in each bin
  # Lower bound
  bin_rows_lb = floor(n_rows/n_folds)
  # Upper bound
  bin_rows_ub = ceiling(n_rows/n_folds)
  # Cycle through the number of folds/bins until we have the
  # total number of rows of the df, then sort the result
  bin_vec = sort(head(rep(1:n_folds, bin_rows_ub), n_rows))
  # Shuffle the rows of the df
  shuff_rows = sample(1:n_rows, n_rows, replace = FALSE)
  # Initialize a list with n_folds elements, each of which will
  # then be populated with indices extraced from shuff_rows
  bins = vector('list',n_folds)
  # Iterate through each fold/bin and populate said bin
  for (i in 1:n_folds) bins[[i]] = df[shuff_rows[bin_vec == i],]
  # Initialize a vector which will contain predicted values
  # of the to_predict feature
  predictions = rep(NA, n_rows)
  # Initialize a counter for populating the predictions vector
  j = 0
  # Iterate through each bin again
  # This time, extract one bin to use as a test set, and combine
  # the rest to use as a cross-validation set
  for (i in 1:n_folds) {
    # Extract the current bin
    test = bins[[i]]
    # Combine the other bins into a cross-validation set
    cv = do.call(rbind, bins[-i])
    # Run the linear model on the cv set
    arg1 = paste(to_predict,"~ .") # Default separation (' ')
    linear_fit = lm(arg1, cv)
    # Populate the predictions vector with predictions
    # based on the test set
    predictions[(j+1):(j+nrow(test))] = predict(linear_fit, test)
    # Update the counter
    j = j+nrow(test)
  }
  # Return the RMSE between predictions and the actual (shuffled)
  # to_predict column of df
  return(rmse(predictions, df[shuff_rows, to_predict]))
}

# Function for calculating the (non-normalized) RMSE between x and y
rmse = function(x,y) {
  # Make sure that x and y have the same length
  if (length(x) != length(y)){
    print(c("x=",length(x), "y=",length(y)))
    stop('x and y must have the same length')}
  return(sqrt(mean((x-y)^2)))
}

feat_targ_scale = data.frame(cbind(target, scale(features)))
# Using nfold cv linear fit
nfold_cv_rmse = nfold_cv(feat_targ_scale, n_folds=6, to_predict='target')
baseline_rmse = rmse(target,df_wide$rough_rating)
model_comparisons = rbind(model_comparisons, c("baseline_RMSE", baseline_rmse))


#### Caret Utility Function ##########
caret_reg = function(x, y, method, grid, ...) {
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=2,
                         number=3, verboseIter=TRUE)
  train(x=x, y=y, method=method, tuneGrid=grid,
        trControl=control, metric="RMSE",
        preProcess=c("center", "scale"), ...)
}

########  List of Regression Models ########

# Ranger
forest_grid = data.frame(mtry = c(6:10))
forest_model = caret_reg(x=scale(features),
                         y=as.numeric(target), 
                         method = 'ranger', 
                         grid = forest_grid, 
                         importance = 'impurity' )


ranger_RMSE = min(forest_model$results$RMSE)
model_comparisons = rbind(model_comparisons, c("ranger_RMSE", ranger_RMSE))

# GLM
glm_features = model.matrix(target~., feat_targ_scale)
alpha=seq(0, 1, 0.1) 
lambda=2**seq(-4, 1,length.out=20)
glmgrid = expand.grid('alpha'=alpha,'lambda'=lambda)
glm_model = caret_reg(x=glm_features,
                      y=target, 
                      method = 'glmnet', 
                      grid = glmgrid )

glm_RMSE = min(glm_model$results$RMSE)


# knn
knn_grid = data.frame(k = c(39:41))
knn_model = caret_reg(x=scale(features),
                      y=target,
                      method = 'knn',
                      grid = knn_grid)


knn_RMSE = min(knn_model$results$RMSE)
model_comparisons = rbind(model_comparisons, c("knn_RMSE", knn_RMSE))

# Mars
marsgrid = expand.grid(degree = c(1:2), nprune = c(1:3))
mars_model = caret_reg(x=features,y=target, method = 'earth', grid = marsgrid )
min(mars_model$results$RMSE)
mars_RMSE = min(mars_model$results$RMSE)
model_comparisons = rbind(model_comparisons, c("mars_RMSE", mars_RMSE))

# Cubist
cubist_grid = expand.grid(committees=seq(41, 48, 1), neighbors=8:9)
# committees=seq(20, 55, 5)
#cubist neighbors must be less than 10
cubist_model = caret_reg(x=features,y=target, method = 'cubist', grid = cubist_grid)
min(cubist_model$results$RMSE)
cubist_RMSE = min(cubist_model$results$RMSE)
model_comparisons = rbind(model_comparisons, c("cubist_RMSE", cubist_RMSE))

# Determine these hyperparameters!
best_forest_grid = data.frame(mtry = 7)
best_glmnet_grid = data.frame(alpha = 0.1, lambda = 0.803)
best_knn_grid = data.frame(k=40)
best_marsgrid = data.frame(nprune = 1, degree = 1)
best_cubist_grid = data.frame(committees=47,neighbors=9)

# Part 2.5: Ensemble those models!
library(caretEnsemble)
ensemble_methods = c('ranger','glmnet', 'knn', 'earth', 'cubist')
ensemble_control = trainControl(method="repeatedcv", repeats=3,
                                number=4, verboseIter=TRUE,
                                savePredictions="final")

ensemble_tunes = list(
  ranger=caretModelSpec(method='ranger', tuneGrid=best_forest_grid),
  glmnet=caretModelSpec(method='glmnet', tuneGrid=best_glmnet_grid),
  earth=caretModelSpec(method='earth', tuneGrid=best_marsgrid),
  knn=caretModelSpec(method='knn', tuneGrid=best_knn_grid),
  cubist=caretModelSpec(method='cubist', tuneGrid=best_cubist_grid)
)


ensemble_fits1 = caretList(features, target,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes,
                          preProcess=c("center", "scale"))

fit_ensemble1 = caretEnsemble(ensemble_fits1)

print(fit_ensemble)
summary(fit_ensemble)

ensemble_RMSE = as.numeric(fit_ensemble1$ens_model$results[2])
model_comparisons = rbind(model_comparisons, c("ensemble_RMSE", ensemble_RMSE))

# Part 3: Predicting based on factor of 4+ rating vs <4 rating
library("e1071")
library("pROC")
baseline_accuracy = sum(as.numeric(rough_rating_factor==target_factor))/length(target_factor)
model_comparisons = rbind(model_comparisons, c("baseline_Accuracy", baseline_accuracy))

fact_data = cbind(target_factor, features)

############  Caret Classification functions ############

caret_reg_ROC = function(x, y, method, grid, ...) {
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=2,
                         number=3, verboseIter=TRUE,
                         summaryFunction = twoClassSummary, classProbs = TRUE)
  train(x=x, y=y, method=method, tuneGrid=grid,
        trControl=control, metric = 'ROC',
        preProcess=c("center", "scale"), ...)
}

caret_reg_accuracy  = function(x, y, method, grid, ...) {
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=2,
                         number=3, verboseIter=TRUE)
  train(x=x, y=y, method=method, tuneGrid=grid,
        trControl=control, metric = 'Accuracy',
        preProcess=c("center", "scale"), ...)
}
  
  
  
# svm
# svm_grid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))
svm_grid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:3))
svm_model = caret_reg_ROC(x=features,
                            y=target_factor,
                            method = 'svmRadial',
                            grid = svm_grid)

svm_model = caret_reg_accuracy(x=features,
                          y=target_factor,
                          method = 'svmRadial',
                          grid = svm_grid)

svm_model$bestTune
svm_ROC = max(svm_model$results$ROC)
svm_accuracy = min(svm_model$results$Accuracy)
# sigma = 1, C = 1
model_comparisons = rbind(model_comparisons, c("svm_Accuracy", svm_accuracy))

# Ranger
forest_grid = data.frame(mtry = c(1:3))
forest_model_ROC = caret_reg_ROC(x=features_train,
                         y=target_factor_train, 
                         method = 'ranger', 
                         grid = forest_grid, 
                         importance = 'impurity' )


ranger_ROC = min(forest_model_ROC$results$ROC)
model_comparisons = rbind(model_comparisons, c("ranger_Accuracy", ranger_Accuracy))
# mtry = 1
forest_model$trainingData$.outcome


pre3 = predict(forest_model_ROC, features_test, type = 'prob')
summary(pre3)
pre3[,2]
pred2 = prediction(pre3[,2], target_factor_test)
perf2 = performance(pred2, 'tpr', 'fpr')
plot(perf2)

# GLM
alpha=seq(0, 0.3, 0.02) 
lambda=2**seq(-4, 0,length.out=20)
glmgrid = expand.grid('alpha'=alpha,'lambda'=lambda)
glm_model = caret_reg_accuracy(x=features,
                      y=target_factor, 
                      method = 'glmnet', 
                      grid = glmgrid )
glm_model$bestTune
glm_Accuracy = min(glm_model$results$Accuracy)
model_comparisons = rbind(model_comparisons, c("glm_Accuracy", glm_Accuracy))
model_comparisons = model_comparisons[-c(12:13),]

# knn
knn_grid = data.frame(k = c(39:41))
knn_model = caret_reg_accuracy(x=features,
                      y=target_factor,
                      method = 'knn',
                      grid = knn_grid)
knn_model
knn_accuracy = max(knn_model$results$Accuracy)
model_comparisons = rbind(model_comparisons, c("knn_Accuracy", knn_accuracy))
model_comparisons = na.omit(model_comparisons)



# Mars
marsgrid = expand.grid(degree = c(1:2), nprune = c(1:3))
mars_model = caret_reg_accuracy(x=features,
                                y=target_factor, 
                                method = 'earth', 
                                grid = marsgrid )
mars_model$bestTune
max(mars_model$results$Accuracy)
mars_Accuracy = max(mars_model$results$Accuracy)
model_comparisons = rbind(model_comparisons, c("mars_Accuracy", mars_Accuracy))

# Naive Bayes
nbgrid = expand.grid(fL = c(0, 0.5,1), usekernel = c(TRUE, FALSE), adjust = c(1:3))
nb_model = caret_reg_accuracy(x=features,
                                y=target_factor, 
                                method = 'nb', 
                                grid = nbgrid )
nb_model$bestTune
max(nb_model$results$Accuracy)
nb_Accuracy = max(nb_model$results$Accuracy)
model_comparisons = rbind(model_comparisons, c("nb_Accuracy", nb_Accuracy))


# Determine these hyperparameters!
best_svm_grid = svm_model$bestTune
best_forest_grid = forest_model$bestTune
best_glmnet_grid = glm_model$bestTune
best_knn_grid = knn_model$bestTune
best_marsgrid = mars_model$bestTune
best_nbgrid = nb_model$bestTune

# Part 3.5: Ensemble those models!

ensemble_methods = c('svmRadial', 'ranger','glmnet', 'knn', 'earth')
ensemble_control = trainControl(method="repeatedcv", repeats=3,
                                number=4, verboseIter=TRUE,
                                savePredictions="final", classProbs = TRUE)

ensemble_tunes = list(
  svm=caretModelSpec(method='svmRadial', tuneGrid=best_svm_grid,
  ranger=caretModelSpec(method='ranger', tuneGrid=best_forest_grid),
  glmnet=caretModelSpec(method='glmnet', tuneGrid=best_glmnet_grid),
  earth=caretModelSpec(method='earth', tuneGrid=best_marsgrid),
  knn=caretModelSpec(method='knn', tuneGrid=best_knn_grid)
  )
)


ensemble_fits = caretList(features, target_factor,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes,
                          preProcess=c("center", "scale"))

fit_ensemble = caretEnsemble(ensemble_fits)

print(fit_ensemble$ens_model$results[2])
summary(fit_ensemble)
fit_df = data.frame(summary(fit_ensemble))
ensemble_accuracy = as.numeric(fit_ensemble$ens_model$results[2])
model_comparisons = rbind(model_comparisons, c("ensemble_Accuracy", ensemble_accuracy))

save_path = "/Users/nathan/Documents/Stats/SignalDataScience/finalproject/"
write.csv(model_comparisons, paste0(save_path, "model_comparisons.csv"), row.names = FALSE)
