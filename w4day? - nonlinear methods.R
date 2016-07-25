df= read.csv('whitewinequal.csv', sep = ";", head = FALSE)
View(df)
as.vector(df[1,1])
names(df)=c('fixed_acidity','volatile_acidity','citric_acid','residual_sugar','chlorides','free_sulpher_dioxide','total_sulpher_dioxide','density','pH','sulphates','alcohol','quality')
df = df [-1,]
row.names(df)=c(1:4898)

df["fixed_acidity"] = as.numeric(as.character(df$fixed_acidity))

for (i in 1:11){
  graph = ggplot(data=df,aes(x=df[[i]],y=df[[12]])) + geom_smooth() + labs(x=names(df)[i],y='quality')
  plot(graph)
}

# caret 
library('caret')
caret_reg = function(x, y, method, grid, ...) {
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=1,
                         number=3, verboseIter=TRUE)
  train(x=x, y=y, method=method, tuneGrid=grid,
        trControl=control, metric="RMSE",
        preProcess=c("center", "scale"), ...)
}

alpha=seq(0, 1, 0.1) 
lambda=2**seq(-4, 1,length.out=20)
grid1 = expand.grid(alpha = alpha,lambda =lambda)
View(grid1)
wine_features = select(df,fixed_acidity:alcohol)
wine_quality = df$quality

wine_model = caret_reg(x=scale(wine_features),y=wine_quality, method = 'glmnet', grid = grid1 )

results = data.frame(results = 0, rmse = 0)

min(wine_model$results$RMSE)
results[1,] = c('caretglmnet',0.7553755)
View(results)


kdf = data.frame(k=c(1:20))

knn_model = caret_reg(x=scale(wine_features),y=wine_quality, method = 'knn', grid = kdf )

min(knn_model$results$RMSE)
knn_model$bestTune
plot(knn_model$results[[2]])
results[2,]=c('knn',min(knn_model$results$RMSE))

#going to mars 

marsgrid = expand.grid(degree = c(1:5), nprune = c(10:30))
mars_model = caret_reg(x=scale(wine_features),y=wine_quality, method = 'earth', grid = marsgrid )
min(mars_model$results$RMSE)
summary(mars_model)
plot(mars_model$results[[3]])
mars_model$results
results[3,] = c('mars',min(mars_model$results$RMSE))

#tree 

tree_grid = data.frame(cp=10**seq(-3, 0,length.out=10))
tree_model = caret_reg(x=scale(wine_features),y=wine_quality, method = 'rpart', grid = tree_grid )
plot(tree_model$finalModel)
text(tree_model$finalModel)
results[4,]= c('tree',min(tree_model$results$RMSE))
    
#random forest 

forest_grid = data.frame(mtry = c(2:6))
forest_model = caret_reg(x=scale(wine_features),y=wine_quality, method = 'ranger', grid = forest_grid, importance = 'impurity' )
forest_model$variable.importance
varImp(forest_model)
predErr(forest_model)
str(forest_model)
forest_model$finalModel$variable.importance
forest_model$finalModel$prediction.error
results[5,]= c('ranger',min(forest_model$results$RMSE))

#boosted trees 
library(gbm)
boosted_grid = expand.grid(n.trees = 500, interaction.depth = c(1,5,10,20,40,60), shrinkage = seq(0.01,0.1,0.03), n.minobsinnode = c(1:3))
boosted_model = caret_reg(x=scale(wine_features),y=wine_quality, method = 'gbm', grid = boosted_grid)
min(boosted_model$results$RMSE[1:60])
results[6,] = c('boosted tree',min(boosted_model$results$RMSE[1:60]))

boosted_grid2 = expand.grid(n.trees = seq(500,5000,500),interaction.depth = 40, shrinkage = 0.04, n.minobsinnode = 3)
boosted_model2 = caret_reg(x=scale(wine_features),y=wine_quality, method = 'gbm', grid = boosted_grid2)
results[7,] = c('better boosted tree',min(boosted_model2$results$RMSE))

#cubism 

cubist_grid = expand.grid(committees=seq(30, 50, 5), neighbors=5:9)
cubist_model = caret_reg(x=scale(wine_features),y=wine_quality, method = 'cubist', grid = cubist_grid)
results[8,]=c('cubist',min(cubist_model$results$RMSE))

#Stacking 
library(caretEnsemble)
ensemble_methods = c('glmnet', 'earth', 'rpart')
ensemble_control = trainControl(method="repeatedcv", repeats=1,
                                number=3, verboseIter=TRUE,
                                savePredictions="final")

ensemble_tunes = list(
  glmnet=caretModelSpec(method='glmnet', tuneGrid=grid1),
  earth=caretModelSpec(method='earth', tuneGrid=marsgrid),
  rpart=caretModelSpec(method='rpart', tuneGrid=tree_grid)
)


ensemble_fits = caretList(wine_features, wine_quality,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes,
                          preProcess=c("center", "scale"))

fit_ensemble = caretEnsemble(ensemble_fits)
print(fit_ensemble)
summary(fit_ensemble)
results[9,] = c('ensemble glm,mars,tree', 0.7079081)

#again, with ranger instead of rpart

ensemble_methods = c('glmnet', 'earth', 'ranger')
ensemble_control = trainControl(method="repeatedcv", repeats=1,
                                number=3, verboseIter=TRUE,
                                savePredictions="final")

ensemble_tunes = list(
  glmnet=caretModelSpec(method='glmnet', tuneGrid=grid1),
  earth=caretModelSpec(method='earth', tuneGrid=marsgrid),
  rpart=caretModelSpec(method='ranger', tuneGrid=forest_grid)
)


ensemble_fits = caretList(wine_features, wine_quality,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes,
                          preProcess=c("center", "scale"))

fit_ensemble2 = caretEnsemble(ensemble_fits)
print(fit_ensemble2)
results[10,] = c('ensemble glm,mars,ranger',   0.6097564)

#fin
