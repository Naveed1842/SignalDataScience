
library('ggplot2')
library('glmnet')
cost = function(x, y, lambda, alpha) {

}

grid = expand.grid(lambda = 2^(-2:7), alpha = seq(from = -0.1, to= 0.3, by= 0.001))
View(grid)

?glm
?predict.glmnet
?step

scal = scale(df)

center_extract = attributes(scal)[['scaled:center']]

scale_extract = attributes(scal)[['scaled:scale']]

scal2 = scale(df, center = center_extract, scale = scale_extract)


