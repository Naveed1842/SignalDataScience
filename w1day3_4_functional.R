unlist(lapply(mtcars[1,],class))
df = data.frame(matrix(nrow=32,ncol=11))
dim(mtcars)
dim(df)
count = 1
lapply(mtcars[,1],function(x) (x - mean(x))/sd(x))


for(col in colnames(mtcars)){
  df[,count] = unlist(lapply(mtcars[,col],function(x) (x - mean(mtcars[,col]))/sd(mtcars[,col])))
  count = count+1
}

str(mtcars)

zscore = function(v){
  m = mean(v)
  sd = sd(v)
  return(unlist(lapply(v, function(v) ((v - m)/sd))))
}
df = data.frame(matrix(1:100,nrow=10))
df[1:5] = lapply(df[1:5], as.character)
zscore(df[,6])

stand_num = function(df){
  for(col in colnames(df)){
    if(is.numeric(df[1,col])){
      print(lapply(df[,col], zscore))
      df[,col]=zscore(df[,col])
    }
  }
  return(df)
}
stand_num(df)
print(df)

# Better version of stand_num
lapply(df, function(x) if(is.numeric(x)){zscore(x)})


one = data.frame(matrix(1,nrow=3,ncol=3))
one - one
length(df)
attributes(one)$

minus = function(df){
  new=data.frame(matrix(nrow=nrow(df),ncol=ncol(df)))
  new[,1]=0
  new[,2:ncol(new)] = df[,1:ncol(df)-1]
  return(df-new)
  #orig = df
  #for(i in 2:length(colnames(orig))){
  #  df[,i]=lapply(df[,i],function(x) x-orig[])
  #}
}
minus(df)


multiply = function (x, k=2){
  k*x
}
sapply(minus(df), multiply, k=10)


sapply(df, function(x) x+2)

?weighted.mean

values = lapply(1:10, function(x) rnorm(10))
values[[2]][[1]] = NA
weights = lapply(1:10, function(x) rnorm(10))

unlist(Map(na.omit(weighted.mean), values, weights))

Map(multiply, values, k=.0004)

sapply(values, sum)
Reduce(`+`, values)
unlist(lapply(values, sum))

union(m, m2)
intersect(m, m2)
