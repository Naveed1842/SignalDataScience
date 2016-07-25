# National Election Study

# Load libraries and data
library("dummies")
library("caret")
library("psych")
library("corrplot")


elect = read.csv("nes_cleaned_1992.csv")


# Expand factors into dummy variables (one less variable per factor)
nonvoters = elect[elect["vote"] != "yes",]
nonvoters[c("vote", "presvote")] = NULL
elect = elect[elect["vote"] == "yes",]
elect["vote"] = NULL
elect = elect[elect["presvote"] == "democrat" | elect["presvote" ] == "republican",]
elect_factors = dummy.data.frame(elect, verbose = TRUE, sep = "_")
nonvoter_factors = dummy.data.frame(nonvoters, verbose = TRUE, sep = "_")
factor_drops = c("gender_male", "race_White", "educ_GradeSchool", "urban_central", "region_ north central", "income_0-16 %ile", "occup1_clerical", "union_no", "religion_other", "presvote_democrat")
elect_factors[factor_drops] = NULL
nonvoter_factors[factor_drops] = NULL
nonvoter_factors[c("gender_NA", "race_NA", "educ_NA", "urban_NA", "region_NA", "income_NA", "occup1_NA", "union_NA", "religion_NA")] = 0

sum(elect_factors["presvote_NA"])
dim(elect_factors)
elect_factors = elect_factors[elect_factors["presvote_NA"] == 0,]
elect_factors$`presvote_NA` = NULL

elect_fit = glm(presvote_republican~., family = "binomial", data = elect_factors)

# Order features by magnitude desc to find most predictive features of voting Republican
elect_fit_coefs = elect_fit$coefficients[2:length(elect_fit$coefficients)]
top5ElectFeatures = elect_fit_coefs[order(abs(elect_fit_coefs), decreasing = TRUE)][1:5]


top5ElectFeatures


elect_predict = predict(elect_fit, newdata = nonvoter_factors)
elect_predict[elect_predict < 0.5] = 0
elect_predict[elect_predict >= 0.5] = 1
elect_predict = as.factor(elect_predict, levels = c(0, 1), labels = c("democrat", "republican"))
percentBush = round(sum(elect_predict)/length(elect_predict) * 100, 1)
percentClinton = 100 - percentBush

summary(elect$presvote)
truePercentClinton = round((765/(765 + 539)) * 100, 1)


# National Merit Twin Study

twin_df = read.csv("NMSQT.csv")

# Set grid of parameter values to search over
param_grid = expand.grid(alpha = 1:10 * 0.1,
                         lambda = 10^seq(-4, 0, length.out=10))
# Set 10-fold cross validation repeated 3x
control = trainControl(method="repeatedcv", number=10, repeats=3, verboseIter=TRUE)

# Create scaled_features and the set the unscaled target
scaled_features = scale(twin_df[5:ncol(twin_df)])
unscaled_target = twin_df$NMSQT

# Search over the grid
caret_fit = train(x=scaled_features,
                  y=unscaled_target,
                  method="glmnet",
                  tuneGrid=param_grid,
                  trControl=control)
# View the optimal values of alpha and lambda
caret_fit$bestTune
# View the cross-validated RMSE estimates
caret_fit$results$RMSE

question_df = read.csv("NMSQTcodebook.csv", stringsAsFactors = FALSE)
question_df['set'] = NULL
str(caret_fit)
?join
library("plyr")
twin_coef = coef(caret_fit$finalModel, caret_fit$bestTune$lambda)
str(twin_coef)
twin_coef_df = data.frame(as.vector(twin_coef), row.names = rownames(twin_coef))



?train
combo_df = cbind(twin_coef_df[2:479,], question_df)
combo_df$set = NULL
colnames(combo_df)[1] = "coefficient"
combo_df = combo_df[order(abs(combo_df$coefficient), decreasing = TRUE),]

# Positive correlations assoc with higher test scores
twin_top5coef = head(combo_df)

# Primary component analysis of Twin df
twin_PCA = prcomp(scaled_features)

qplot(twin_PCA$scale)



topFivePCA = function(PCA_obj){
  topFivePCA_list = matrix(data = NA, nrow = 50, ncol = 2)
  for(i in 1:5){
    rotation = PCA_obj$rotation[,i]
    loadings = rotation[order(abs(rotation), decreasing = TRUE)]
    topFivePCA_list[(((i-1)*10)+1):(i*10),1] = loadings[1:10]
    topFivePCA_list[(((i-1)*10)+1):(i*10),2] = names(loadings)[1:10]
  }
  topFivePCA_df = data.frame(topFivePCA_list)
  colnames(topFivePCA_df) = c("loadings", "var")
  return(topFivePCA_df)
}


twin_top5PCA = topFivePCA(twin_PCA)
PCAloadings_combo = join(twin_top5PCA, question_df, by = "var")

# Eigenvalue Plot
eigenvals = data.frame(seq_along(twin_PCA$sdev), twin_PCA$sdev)
ggplot(data = eigenvals) + geom_point(aes(x=eigenvals[1],y=eigenvals[2]))
ggplot(data = eigenvals) + geom_point(aes(x=eigenvals[1],y=eigenvals[2])) + xlim(0,12)
# Picking PCs 1-7 based on the plot

# Factor Analysis with 7 oblique factors
twin_fa = fa(scaled_features, nfactors = 7, rotate = "oblimin")

topFactors = function(fa_obj, n){
  topFactors_list = matrix(data = NA, nrow = 10*n, ncol = 3)
  for(i in 1:n){
    loadings_unsorted = fa_obj$loadings[,i]
    loadings = loadings_unsorted[order(abs(loadings_unsorted), decreasing = TRUE)]
    topFactors_list[(((i-1)*10)+1):(i*10),1] = loadings[1:10]
    topFactors_list[(((i-1)*10)+1):(i*10),2] = names(loadings)[1:10]
    topFactors_list[(((i-1)*10)+1):(i*10),3] = rep(i, 10)
  }
  topFactors_df = data.frame(topFactors_list)
  colnames(topFactors_df) = c("loadings", "var", "factor")
  return(topFactors_df)
}


twin_topFactors = topFactors(twin_fa, 7)
loadings_combo = join(twin_topFactors, question_df, by = "var")


loadings_combo['factor'] = as.factor(loadings_combo['factor'])
levels(loadings_combo$factor) = c("depression", "introversion", "neuroticism", "dishonesty", "physical_adventure", "naughty_socialite", "leadership")

# Aggregating the factors based on gender
twin_gender = aggregate(twin_fa$scores, by = twin_df['SEX'], mean)
colnames(twin_gender) = c("gender","depression", "introversion", "neuroticism", "dishonesty", "physical_adventure", "naughty_socialite", "leadership")

twin_fa_df = data.frame(twin_df[1:4], twin_fa$scores)
head(twin_fa_df)
colnames(twin_fa_df)[5:11] = c("depression", "introversion", "neuroticism", "dishonesty", "physical_adventure", "naughty_socialite", "leadership")



# For a given variable, percent variance explained by additive genetic effects is
# 2*(r_MZ - r_DZ)






