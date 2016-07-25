# Self assessment 2: July 18 10:18am

library("dplyr")
library("psych")
library("ggplot2")
library("glmnet")


fill_na = function (df) {
  if(sum(is.na(df))>0){
    for(col in colnames(df)){
      common = as.numeric(names(sort(table(df[,col]), decreasing = TRUE)[1]))
      df[is.na(df[,col]),col] = common
    }
  }
  return(df)
}

# Set Random Seed for reproducibility
set.seed(1)

# Set alpha and lambda search space
alpha_set = seq(0, 1, by = 0.1)
lambda_set = 10 ** seq(1, -3, by = -(4/49))

# Setup dataframes
df = fill_na(msq)
features = select(df, active:scornful)
extraversion = df["Extraversion"]
neuroticism = df["Neuroticism"]

# Generate fold assignments
num_of_folds = 10
set.seed(1)
groups = (sample(1:nrow(df)) %% num_of_folds) + 1

# Initialize four lists
feature_train_groups = list(rep(NA, num_of_folds))
feature_test_groups = list(rep(NA, num_of_folds))
extrav_groups = list(rep(NA, num_of_folds))
neurot_groups = list(rep(NA, num_of_folds))

results_matrix = matrix(data = NA, nrow = length(lambda_set)*length(alpha_set), ncol = 4)

for(i in 1:num_of_folds){
  # Fill these four lists for each fold
  # Scale features on train, and pass scale to test
  feature_train_groups[i] = list(scale(features[groups != i,]))
  temp_center = attr(feature_train_groups[i][[1]], "scaled:center")
  temp_scale = attr(feature_train_groups[i][[1]], "scaled:scale")
  feature_test_groups[i] =
    list(scale(features[groups == i,],
    center = temp_center,
    scale = temp_scale))
  extrav_groups[i] = list(extraversion[groups != i,])
  neurot_groups[i] = list(neuroticism[groups != i,])
}

rmse = function(x,y){
  # root-mean-square-error
  if(length(x) != length(y)) {print(c("length x:", length(x), "length y: ", length(y)));warning("RMSE: x and y not same length")}
  diff_xy = c()
  for(i in 1:(min(length(x),length(y)))){
    diff_xy[i] = x[i] - y[i]
  }

  numerator = sum(unlist(diff_xy), na.rm = TRUE)**2
  denom = length(diff_xy)
  return(sqrt(numerator/denom))
}

results_count = 1
extrav_groups_length = length(unlist(extrav_groups))
neurot_groups_length = length(unlist(neurot_groups))
# Grid Search
for(alpha in alpha_set){
  print(c("alpha = ", alpha))
  extrav_fit_list = list(rep(NA, num_of_folds))
  neurot_fit_list = list(rep(NA, num_of_folds))
  for(i in 1:num_of_folds){
    #print(c("str(feature train)", str(feature_train_groups[i][[1]])))
    #not null
    #print(c("str(extrav_groups)", str(extrav_groups[i][[1]])))
    #not null
    extrav_fit_list[i] =
      list(
      glmnet(feature_train_groups[i][[1]],
      extrav_groups[i][[1]],
      alpha = alpha, lambda = lambda_set))
    neurot_fit_list[i] =
      list(
      glmnet(feature_train_groups[i][[1]],
      neurot_groups[i][[1]],
      alpha = alpha, lambda = lambda_set))
  }
  #count_alpha = count_alpha + 1
  for(lambda in lambda_set){
    print(c("lambda = ", lambda))
    extrav_predict = list(rep(NA, 10))
    neurot_predict = list(rep(NA, 10))
    for(i in 1:num_of_folds){
      # Fill in predictions vectors according to folds (groups)
      # predict(fit, features, s = lambda)
      extrav_predict[i] =
        predict(extrav_fit_list[i][[1]],
        feature_test_groups[i][[1]],
        s = lambda)

      #predict(extrav_fit_list[i][[1]]


      neurot_predict[i] =
        predict(neurot_fit_list[i][[1]],
        feature_test_groups[i][[1]], s = lambda)

    }
    # Calculate rmse for extrav and neurot
    rmse_e = rmse(unlist(unlist(extrav_predict)), unlist(extrav_groups))
    rmse_n = rmse(unlist(unlist(neurot_predict)), unlist(neurot_groups))
    # assign to results matrix
    results_matrix[results_count,] = c(alpha, lambda, rmse_e, rmse_n)
    results_count = results_count + 1
  }
}

results = as.data.frame(results_matrix)
colnames(results) = c("Alpha", "Lambda", "Extraversion RMSE", "Neuroticism RMSE")


arg_min = function (v) {
  which.min(v)
}


extr_min = arg_min(results$`Extraversion RMSE`)
extr_vals = results[extr_min, 1:3]



neur_min = arg_min(results$`Neuroticism RMSE`)
neur_vals = results[neur_min, c(1:2,4)]


extrav_full_fit = glmnet(x = features, y = df['Extraversion'], alpha = extr_vals$Alpha, lambda = extr_vals$Lambda)



?glmnet
