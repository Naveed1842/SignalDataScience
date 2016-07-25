df = data.frame(matrix(1:100, nrow = 10))
names(df)
nrow(df)
ncol(df)
rownames(df)
colnames(df)
dim(df)
cbind(df, df)

tenByTwenty = rbind(df, df)
fourtyBy10 = cbind(df, df, df, df)
abc = c("a", "b", "c", "a")
is.character(abc)
df2 = data.frame(abc)
str(df2)
str(df)
x = 1:5
print(x)
x[c(3,1)]
x[-c(3,1)]
x[c(-3,-1)]
x[c(1,1, 3)]
x[c(TRUE, FALSE, TRUE, TRUE)]

arg_max = function(v){
  # returns position of greatest element
  maxVal = max(v, na.rm = TRUE)
  return(match(maxVal, v))
}

arg_max(df)
inb = list(1, "a")
bah = list(inb, "b", c(3, 4, 5, 2))

longest_run = function(v){
  # returns longest sequence of consecutive identical values

}

getSamples = function (a, n){
  x = rnorm(n, mean = 0, sd = 1)
  error = rnorm(n, mean = 0, sd = ((1-(a^2))^(1/2)))
  y = a*x + error
  df = data.frame(x, y)
  return(df)
}
df = getSamples(a = 0.4, n = 100)
ggplot(data = df, aes(x=x, y=y)) + geom_point() + geom_smooth()

v = c()
