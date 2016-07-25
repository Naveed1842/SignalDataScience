df=read.csv('biketrain.csv')

colsplit=unlist(strsplit(as.character(df[,1]), split = ' ', fixed = TRUE))
times = colsplit[seq_along(colsplit) %% 2 == 0]
dates = colsplit[seq_along(colsplit) %% 2 == 1]

time_split = unlist(strsplit(times, split = ":", fixed = TRUE))
hour = time_split[seq_along(time_split) %% 3 == 1]

df=cbind(hour,df)
df$datetime = NULL
df$casual = NULL
df$registered = NULL

df["hour"] = as.numeric(as.character(df$hour))
featuresfull = df[1:9]
target = df$count


fullerfeatures = dummy.data.frame(data=featuresfull, names = c('season','weather'))
fullerfeatures$season4=NULL
fullerfeatures$weather4 = NULL
fullerfeatures$atemp = NULL


library(forecast)
humiditylambda = BoxCox.lambda(fullerfeatures$humidity, method = 'guerrero')
humidbox = BoxCox(fullerfeatures$humidity,humiditylambda)
fullerfeatures$humidity = humidbox

windspeedlambda = BoxCox.lambda(fullerfeatures$windspeed, method = 'guerrero')
windybox = BoxCox(fullerfeatures$windspeed, windspeedlambda)
fullerfeatures$windspeed = windybox


#caret utility function 
library('caret')
caret_reg = function(x, y, method, grid, ...) {
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=1,
                         number=3, verboseIter=TRUE)
  train(x=x, y=y, method=method, tuneGrid=grid,
        trControl=control, metric="RMSE",
        preProcess=c("center", "scale"), ...)
}

forest_grid = data.frame(mtry = c(5:8))
forest_model = caret_reg(x=scale(fullerfeatures),
                         y=target, 
                         method = 'ranger', 
                         grid = forest_grid, 
                         importance = 'impurity' )


forest_model$results$RMSE
#mtry = 7 returns best rmse 

for(i in 1:5){
  df[i] = as.factor(as.character(df[i]))
  
}
str(df)
glm_subset = df[c()]
glm_features = model.matrix(count~., df)

alpha=seq(0, 1, 0.1) 
lambda=2**seq(-4, 1,length.out=20)
glmgrid = expand.grid('alpha'=alpha,'lambda'=lambda)
glm_model = caret_reg(x=scale(fullerfeatures),
                         y=target, 
                         method = 'glmnet', 
                         grid = glmgrid )

glm_model$results$RMSE

#knn
knn_grid = data.frame(k = c(2:6))
knn_model = caret_reg(x=scale(fullerfeatures),
                         y=target, 
                         method = 'knn', 
                         grid = knn_grid)


knn_model$results$RMSE
#k = 4 returns best RMSE 

#mars
marsgrid = expand.grid(degree = c(1:5), nprune = c(10:30))
mars_model = caret_reg(x=scale(fullerfeatures),y=target, method = 'earth', grid = marsgrid )
min(mars_model$results$RMSE)

plot(mars_model$results[[3]])
mars_model$results
results[3,] = c('mars',min(mars_model$results$RMSE))

ggplot(data = fullerfeatures ,aes(x=windspeed)) + geom_histogram(binwidth = 1)


# Cubist

cubist_grid = expand.grid(committees=seq(30, 50, 5), neighbors=5:9)
cubist_model = caret_reg(x=scale(fullerfeatures),y=target, method = 'cubist', grid = cubist_grid)
min(cubist_model$results$RMSE)
#best rmse (67.63) @ comittees = 40, neighbors = 7

# Stick them all together

best_forest_grid = data.frame(mtry = 7)
best_marsgrid = data.frame(nprune = 17, degree = 2)
best_knn_grid = data.frame(k=4)
best_cubist_grid = data.frame(committees=40,neighbors=7)


library(caretEnsemble)
ensemble_methods = c('ranger', 'knn', 'earth', 'cubist')
ensemble_control = trainControl(method="repeatedcv", repeats=1,
                                number=3, verboseIter=TRUE,
                                savePredictions="final")

ensemble_tunes = list(
  ranger=caretModelSpec(method='ranger', tuneGrid=best_forest_grid),
  earth=caretModelSpec(method='earth', tuneGrid=best_marsgrid),
  knn=caretModelSpec(method='knn', tuneGrid=best_knn_grid),
  cubist=caretModelSpec(method='cubist', tuneGrid=best_cubist_grid)
)


ensemble_fits = caretList(fullerfeatures, target,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes,
                          preProcess=c("center", "scale"))

fit_ensemble = caretEnsemble(ensemble_fits)
print(fit_ensemble)
summary(fit_ensemble)

df=read.csv('kagglebikestest.csv')


colsplit=unlist(strsplit(as.character(df[,1]), split = ' ', fixed = TRUE))
times = colsplit[seq_along(colsplit) %% 2 == 0]
dates = colsplit[seq_along(colsplit) %% 2 == 1]

time_split = unlist(strsplit(times, split = ":", fixed = TRUE))
hour = time_split[seq_along(time_split) %% 3 == 1]

df=cbind(hour,df)
df$datetime = NULL
df$casual = NULL
df$registered = NULL

df["hour"] = as.numeric(as.character(df$hour))
features = df[c(1,2,6:9)]
featuresfull = df[1:9]
target = df$count


fullerfeatures = dummy.data.frame(data=featuresfull, names = c('season','weather'))
fullerfeatures$season4=NULL
fullerfeatures$weather4 = NULL
fullerfeatures$atemp = NULL

humidbox = BoxCox(fullerfeatures$humidity,humiditylambda)
fullerfeatures$humidity = humidbox

windybox = BoxCox(fullerfeatures$windspeed, windspeedlambda)
fullerfeatures$windspeed = windybox

testingfullerfeatures = fullerfeatures

testpredictions = predict(fit_ensemble,testingfullerfeatures)
testpredictions

submission=read.csv('sampleSubmission.csv')
submission$count=testpredictions
submission$count=round(submission$count)
submission$count[submission$count < 0] = 0 


write.csv(submission,file='bikesubmission2.csv', row.names = FALSE)

