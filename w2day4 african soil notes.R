# African Soil Property Prediction
# Data from Kaggle Challenge

str(soil_training)

features =

target =

# Set grid of parameter values to search over
param_grid = expand.grid(.alpha = 1:10*0.1, .lambda = 10^seq(-4, 0, length.out=10))

# Set 10-fold cross validation repeated 3x
control =trainControl(method="repeatedcv", number=10,repeats=3, verboseIter=TRUE)

# Search over the grid

caret_fit =train(x=features, y=target, method="glmnet",tuneGrid=param_grid, trControl=control)

# View the optimal values of alpha and lambda

caret_fit$bestTune

# View the cross-validated RMSE estimates

caret_fit$results$RMSE

subset_df = rbind(bands, target = target)
NB_formula = as.formula(paste0(names(target), ~.))
NB_model = naiveBayes(NB_formula, data = subset_df)
NB_prediction = predict(NB_model, newdata = test_bands)

P.df = cbind(targets['P'], logP = log(targets['P']+1))
ggplot(P.df) + geom_histogram(aes(x=P), color='black') + geom_histogram(aes(x=logP), color='red')

