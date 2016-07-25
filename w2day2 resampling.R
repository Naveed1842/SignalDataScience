# Resampling
library("plyr")
library("stats")
library("ggplot2")
# Random Time Seed
# time = proc.time()[[1]]
# set.seed(time*100)


# Remember to set seed
set.seed(5)


fill_na = function (df) {
  if(sum(is.na(df))>0){
    for(col in colnames(df)){
      common = as.numeric(names(sort(table(df[,col]), decreasing = TRUE)[1]))
      df[is.na(df[,col]),col] = common
    }
  }
  return(df)
}

split_data = function(df){
  df$grouping = sample(2, (dim(df)[1]), replace = TRUE)
  train = subset(df, df$grouping %% 2 == 0)
  test = subset(df, df$grouping %% 2 != 0)
  return(list(train = train, test = test))
}

trim_test = function(df){
  subset_df = df[,c(2, 9:25)]
  return(subset_df)
}

split_predict = function(train, test){
  fitTrain = lm(attr_o~., train)
  train = predict(fitTrain, train)
  test = predict(fitTrain , test)

  return(list(train = train, test = test, p_vals = list(summary(fitTrain)$coefficients[,4])))
}
#prediction2 = nfold_cv(df, 2)





rank_coef = function(train, test){
  train = coef(train)
  test = coef(test)
  train = data.frame(train, abs(train))
  test = data.frame(test, abs(test))
  train = train[order(train[,2], decreasing = TRUE),1]
  test = test[order(test[,2], decreasing = TRUE),1]
  return(list(train = train, test = test))
}

#df_coef = rank_coef(df_predict$train, df_predict$test)
#print(df_coef)

rmse = function(x,y){
  # root-mean-square-error
  diff_xy = c()
  for(i in 1:(min(length(x),length(y)))){
    diff_xy[i] = x[i] - y[i]
  }

  numerator = sum(unlist(diff_xy), na.rm = TRUE)**2
  denom = length(diff_xy)
  return(sqrt(numerator/denom))
}
#rmse(df$train, df$test)

multiSplit = function(df, n){
  output = c()
  for(i in 1:n){
    df2 = split_data(df)
    df_p = (split_predict(df2$train, df2$test))
    output[i] = rmse(df_p$train, df_p$test)
  }
  return(output)
}



n = 100
#results = multiSplit(df, n)


#mean(results)
#standarderr = sd(results)/sqrt(n)
#qplot(results, bins = 50)


# n-fold

nfold_cv = function(df, n_folds){
  df = fill_na(df)
  n_folds_m = matrix(data = 1:n_folds, nrow = dim(df)[1], ncol = 1)
  df$grouping = sample(n_folds_m, (dim(df)[1]), replace = FALSE)
  predictions = list()
  p_vals = c()
  for(i in 1:n_folds){
    chosen = df[df$grouping ==i,]
    others = df[df$grouping !=i,]
    df_p = split_predict(chosen, others)
    predictions[i] = df_p$test
    p_vals[i] = df_p$p_vals
  }
  # position of col with biggest p
  p_vals = data.frame(p_vals)
  big_p_col = rowMeans(p_vals, na.rm = TRUE)
  big_p_col = names(sort(big_p_col, decreasing = TRUE))[1]
  print(big_p_col)
  return(list(rmse(df["attr_o"], predictions), big_p_col))
}
#print(names(p_vals)[4])

#prediction2 = nfold_cv(df, 2)
#prediction10 = nfold_cv(df, 10)




rep_nfold = function(df, n_folds, reps){
  rmse_vector = c()
  for(i in 1:reps){
    rmse_vector[i] = nfold_cv(df, n_folds)
  }
  return(rmse_vector)
}

#predictions_2fold = rep_nfold(df, 2, 100)
#predictions_10fold = rep_nfold(df, 10, 100)
#p2fold = data.frame(unlist(predictions_2fold))
#p10fold = data.frame(unlist(predictions_10fold))
#colMeans(p2fold)
#colMeans(p10fold)
#standard err
#sd(p2fold[,1])/sqrt(100)
#sd(p10fold[,1])/sqrt(100)

#qplot(p2fold, bins=25) + geom_density()
#qplot(p10fold, bins=25) + geom_density()
#ggplot(data = p2fold, aes(x = p2fold, fill = "red", alpha=0.7)) + geom_density() + geom_density(data = p10fold, aes(x = p10fold, fill = "blue")) + theme_dark()

# Stepwise Linear Regression

backward_step = function(df){
  best_rmse = 0
  best_num_vars = 0
  num_var_out = c()
  predictions = c()
  vars = length(names(df))
  print(c("max vars =", vars))
  for(i in 1:(vars-1)){
    predic = nfold_cv(df, 10)
    print(predic[[1]])
    if(predic[[1]]>best_rmse){
      best_rmse = predic[[1]]
      best_num_vars = vars-i
    }
    predictions[(vars-i)] = predic[[1]]
    num_var_out[(vars-i)] = i
    df = df[,!names(df) == predic[[2]]]
  }
  print(c("best model: ", best_num_vars, "vars, with rmse =", best_rmse))
  return(data.frame(predictions, num_var_out))
}



#backstep_p = backward_step(df)
#backstep_p = backstep_p[order(backstep_p[,2], decreasing = TRUE),]
#ggplot(backstep_p, aes(x=num_var_out, y=predictions))  + geom_text(aes(label = num_var_out))


# Using R's step()
df = speeddating.aggregated
df = fill_na(df)
# for vars 2:6
result_df = list()
for(i in 2:6){
  df_sub = df[,c(i, 9:(dim(df)[2]))]
  var_name = attr(df_sub,"names")[1]
  print(var_name)
  form = as.formula(paste(var_name, "~. "))
  model_init = lm(form, df_sub)
  model = formula(model_init)
  step_reg = step(model_init, model, direction = "backward")
  result_df[names(df)[i]] = list(coef(step_reg))
}

#print(result_df)


bootstrap_bad = function(df, approach){
  rmse_v = c()
  rows = dim(df)[1]
  for(i in 1:100){
    picks = sample(rows, rows, replace = TRUE)

    for(j in 1:rows){
      if(j == 1){
        selected_rows = data.frame(df[picks[j],])
      }

      selected_rows = rbind(selected_rows, df[picks[j],])
    }

    # debug print
    # print(str(selected_rows))

    if(approach == 1){
      # Approach type: train=bootstrap, test=df
      df_p = split_predict(train = selected_rows, test = df)
      rmse_v[i] = rmse(df_p$test, df$attr_o)
    } else{
      if(approach == 2){
        # Approach type: train=df, test=bootstrap
        df_p = split_predict(train = df, test = selected_rows)
        rmse_v[i] = rmse(df_p$test, df$attr_o)
      }
    }
  }

  return(mean(rmse_v))
}

# boot_train = bootstrap_bad(df, approach = 1)
# boot_test = bootstrap_bad(df, approach = 2)

bootstrap_good = function (df) {
  rmse_v = c()
  rows = dim(df)[1]
  for(i in 1:3){
    picks = sample(rows, rows, replace = TRUE)
    un_picks = setdiff(picks, rows)

    for(j in 1:rows){
      if(j == 1){
        selected_rows = data.frame(df[picks[j],])
      }

      selected_rows = rbind(selected_rows, df[picks[j],])
    }
    out_of_bag = data.frame(df[un_picks,])
    # Approach type: train=bootstrap, test=df
    df_p = split_predict(train = selected_rows, test = out_of_bag)
    rmse_v[i] = rmse(df_p$test, df$attr_o)
  }
  return(mean(rmse_v))
}

  df = read.csv("~/Documents/Dropbox/Stats Programming/Signal Data Science Nathan/speeddating-aggregated.csv")
  df = fill_na(df)
  df = trim_test(df)
  set.seed(5)

 boot_good = bootstrap_good(df)

