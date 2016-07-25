

#2d simulated data generator

lin_pair = function(m,b,label) {
  if(typeof(m) == "character" | typeof(b) == "character") stop("Non-numeric inputs, fool.")
  x = runif(1, min=0, max=1)
  y = runif(1, min=0, max=1)
  #noise = runif(1, min = 0, max = (1-(m*x+b)))
  if (label == 1){
    while(y<(m*x+b)){
      x = runif(1, min=0, max=1)
      y = runif(1, min=0, max=1)
    }
  } else if (label == 0){
      while(y>(m*x+b)){
        x = runif(1, min=0, max=1)
        y = runif(1, min=0, max=1)
      }
    } else {
      stop('try again, idiot')
    }
  return(c(x,y))
  }

pairs = data.frame(x=0,y=0)
for(i in 1:30){
  output = c()
  output = lin_pair(3,-0.3, 1)
  print(output)
  pairs[i,1] = output[1]
  pairs[i,2] = output[2]
}

ggplot(data = pairs, aes(x = x, y = y)) + geom_point() + xlim(0,1)


quad_pair = function(a,b,c,label){
  x = runif(1, min=0, max=1)
  y = runif(1, min=0, max=1)
  #noise = runif(1, min = 0, max = (1-(m*x+b)))
  if (label == 1){
    while(y<(a*(x-b)**2+c)){
      x = runif(1, min=0, max=1)
      y = runif(1, min=0, max=1)
    }
  } else if (label == 0){
    while(y>(a*(x-b)**2+c)){
      x = runif(1, min=0, max=1)
      y = runif(1, min=0, max=1)
    }
  } else {
    stop('try again, idiot')
  }
  return(c(x,y))
}

quadpairs = data.frame(x=0,y=0)
for(i in 1:50){
  output = c()
  output = quad_pair(3,0.5,-0.3 , 0)
  print(output)
  quadpairs[i,1] = output[1]
  quadpairs[i,2] = output[2]
}
ggplot(data = quadpairs, aes(x = x, y = y)) + geom_point() + xlim(0,1) + ylim(0,1)

#onto Iris

df_iris = iris
View(df_iris)
?iris

ggplot(df_iris,aes(Petal.Length, Petal.Width, color=Species))+ geom_point()

#wines

winedf = read.csv('winecultivars.csv')
winemeans = aggregate(winedf, by = winedf['Type'], FUN = 'mean')
View(winemeans)

sim_data = data.frame(x=rep(NA, 200), y=rep(NA,200))
for(i in 1:100){
  sim_data[i,] = lin_pair(0.75, 0.05, 1)
}
for(i in 101:200){
  sim_data[i,] = lin_pair(0.75, 0.05, 0)
}
sim_data$class=as.factor(c(rep(1,100), rep(0,100)))
View(sim_data)
ggplot(data = sim_data, aes(x=x,y=y, color = class)) +geom_point()


sim_quaddata = data.frame(x=rep(NA, 200), y=rep(NA,200))
for(i in 1:100){
  sim_quaddata[i,] = quad_pair(4, 0.5,0.4, 1)
}
for(i in 101:200){
  sim_quaddata[i,] = quad_pair(4, 0.5,0.38, 0)
}
sim_quaddata$class=as.factor(c(rep(1,100), rep(0,100)))
View(sim_quaddata)
ggplot(data = sim_quaddata, aes(x=x,y=y, color = class)) +geom_point()
library(klaR)
party_matt = partimat(sim_data[1:2], sim_data$class, method="lda")
quad_style = partimat(sim_quaddata[1:2], sim_quaddata$class, method="qda")

library("MASS")
linear_wine = lda(winedf[-1], winedf$Type)
plot(linear_wine)
predict(linear_wine, winedf[-1])$class
output = data.frame(predict(linear_wine), g=winedf$Type)
ldahist(data=predict(linear_wine), g=winedf[1])
winedf[1]
?ldahist
str(linear_wine)


ldahist(predict(linear_wine)$x,g=winedf[[1]])
#^this one worked

linear_wine$scaling[,2]
wine_lds = data.frame(predict(linear_wine)$x)
wine_lds$type = as.factor(winedf$Type)

ggplot(data= wine_lds, aes(x=LD1,y=LD2, color = type)) + geom_point()


#perceptron / percepticon
#generating data

percep_pairs = data.frame(x=0,y=0)
for(i in 1:3000){
  output = c()
  output = lin_pair(1.5, 0.2, 1)
  percep_pairs[i,1] = output[1]
  percep_pairs[i,2] = output[2]
}

for(i in 3001:6000){
  output = c()
  output = lin_pair(1.5, 0.05, 0)
  percep_pairs[i,1] = output[1]
  percep_pairs[i,2] = output[2]
}

percep_pairs$label =c(rep(1,3000),rep(-1,3000))


ggplot(data = percep_pairs, aes(x=x,y=y,color=label)) +geom_point()

percep_pairs$intercept = rep(1,6000)

dot = function (x,y){
  if(length(x) == 1){
    sum(x*y)
  } else {stop("oops, x should be length 1")}
}

perceptron = function (xs, y, w, rate, seed){
  set.seed(seed)
  grouping = sample(nrow(xs), nrow(xs), replace=FALSE)
  samplex = xs[grouping,]
  scramble_y = y[grouping]
  for (i in 1:nrow(samplex)){
    for(j in 1:length(w)){
      temp_xi = dot(samplex[i,j],w)
      if(sign(temp_xi) != scramble_y[i]){
        if(sign(temp_xi) == 1){
          w[j] = w[j] - rate*samplex[i,j]  # false positive
        } else {
          w[j] = w[j] + rate*samplex[i,j]  # false negative
        }
      }
    }
  }
  print(w)
  return(w)
}

perceptron_plot = function(xs, y, w){
  xs$labels = y
  # y=(-w[1]/w[2])x + (-w[3]/w[2])
  b = (-1*w[3]/w[2])
  slope = (-1*w[1]/w[2])
  print(ggplot(data = xs, aes(x=x, y=y, color=labels)) + geom_point() + geom_abline(intercept = b, slope = slope, size = 1.1, color="red"))
}
?geom_abline
w = c(.1,2,.2)
for(i in 1:20){
    w = perceptron(percep_pairs[-3], percep_pairs[[3]], w = w, rate=1, seed = 7)
}


perceptron_plot(percep_pairs[1:2], percep_pairs[3], w)


# Support Vector Machines

# svm() defaults to radial kernel
# so for linear, specify kernel='linear'

library("e1071")

svm_df = pairs[-4]
svm_df$label = as.factor(svm_df$label)


sim_quaddata = data.frame(x=rep(NA, 200), y=rep(NA,200))
for(i in 1:100){
  sim_quaddata[i,] = quad_pair(3, 0.5,0.55, 1)
}
for(i in 101:200){
  sim_quaddata[i,] = quad_pair(3, 0.5,0.4, 0)
}

sim_quaddata$label=as.factor(c(rep(1,100), rep(0,100)))

svm_fit2 = svm(label~., data = sim_quaddata, kernel = "radial", cost=1)

plot(svm_fit2, sim_quaddata)


svm_fit = svm(class~., data = sim_quaddata, kernel = "radial", cost=1)

plot(svm_fit, svm_df)

?svm
dim(winedf)
str(winedf)

winedf$Type = as.factor(winedf$Type)
winefit = svm(Type~., data = winedf, kernel = 'radial')
plot(winefit, winedf, Nonflavanoids~Alcohol)

predict(winefit, winedf)



summary(winefit)
predict(winefit)

str(df_iris)

iris_fit = svm(Species~., data = df_iris, kernel = 'radial')
plot(iris_fit, df_iris, Sepal.Length~Petal.Length)
?plot.svm
?svm
p = predict(iris_fit, df_iris)
str(p)
p

levels(df_iris$Species)
