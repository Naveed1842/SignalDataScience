#Nathan Helm Burger
# July 11th, 10:14am

library("ggplot2")
library("psych")
library("dplyr")
library("hexbin")
library("RColorBrewer")

# Part 1
euclid = function(k = 100){
  x = runif(k, min = 0, max = 1)
  y = vector(mode = "numeric", length = k)
  for(i in 1:k){
    y[i] = runif(1, min = x[i], max = 1)
  }
  return(data.frame(x, y))
}
df1 = euclid(10**5)
qplot(df1, binwidth = 0.05, xmin = 0, xmax = 1)

df1$yCalc = runif(10**5, min = 0, max = 1)
df1$xCalc = (df1["yCalc"] - 1) / log(df1["yCalc"])
p1 = ggplot(df1, aes(x = x, y = y)) + stat_binhex() + geom_smooth(data = df1, aes(x= xCalc, y=yCalc, color = "red"))
p1
sim_data = data.frame(matrix(rnorm(10000), ncol = 2))
purples = brewer.pal(n=7, name = "Purples")
ggplot(sim_data, aes(x=sim_data[,1], y=sim_data[,2])) + stat_binhex() + theme_dark()

# scale_fill_gradientn(colors = purples, trans = "log10")
# with ggplot and stat_binhex()
# scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks)
my_breaks = c(2,4, 8,16, 32, 64, 80)
ggplot(sim_data, aes(x=sim_data[,1], y=sim_data[,2])) + stat_binhex() + theme_dark() + scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks)




bins_w = function(w, df){
  x = df[order(df[,2]),1]
  bins = as.integer(1/w)
  bin_size = floor(length(x)/bins)
  meansX = vector(mode= "numeric", length = as.numeric(bins))
  binsY = vector(mode = "numeric", length = as.numeric(bins))
  temp_length = 0
  for(i in 1:bins){
    temp_x = x[((i-1)*bin_size):((i*bin_size))]
    meansX[i] = mean(temp_x, na.rm = TRUE)
    binsY[i] = (((i-1)+i)/2)*w
    temp_length = temp_length + length(temp_x)
  }
  print(c(dim(df), temp_length))
  binsY <<- binsY
  return(meansX, binsY)
}

meansX = bins_w(w= 0.01, df1)
summary(meansX)
binsY
ggplot(data = data.frame(meansX, binsY), aes(x = meansX, y=binsY)) + geom_point() + geom_smooth(data = df1, aes(x= xCalc, y=yCalc, color = "red"))

# Part 2
View(msq)


str(msq)
msqNA = sapply(msq, function(x) sum(is.na(x)/length(x)))
table(msqNA)
msqNA = sort(msqNA, decreasing = TRUE)
msqNA
msq2 = msq
for(i in 1:ncol(msq2)){
  msq2[is.na(msq2[,i]), i] = mean(msq2[,i], na.rm = TRUE)
}
sum(is.na(msq2))
sum(is.na(msq))

# Histograms
ggplot(msq2, aes(x=Extraversion)) + geom_histogram(aes(fill=..count..)) + scale_fill_gradient("Count", low = "blue", high = "red")
ggplot(msq2, aes(x=Neuroticism)) + geom_histogram(aes(fill=..count..)) + scale_fill_gradient("Count", low = "blue", high = "red")

# attempt at overlay
ggplot(msq2, aes(x=Extraversion)) + geom_histogram() + geom_histogram() + geom_histogram(data = msq2, aes(x=Neuroticism, colour="blue", alpha=0.65))


# Density plots
ggplot(msq2, aes(x=Extraversion)) + geom_density()
ggplot(msq2, aes(x=Neuroticism)) + geom_density()

# Scatter plot
ggplot(msq2, aes(x=Extraversion, y=Neuroticism)) + geom_point() + geom_smooth()

# Linear Regression
colnames(msq2)
colnames(msq2)[80]
colnames(msq2)[81]
fitEx = lm(Extraversion ~ . , data=subset(msq2, select = c(1:80, 82:84)))
fitNe = lm(Neuroticism ~ . , data=subset(msq2, select = c(1:79, 81, 82:84)))
exCoef = data.frame(coef(fitEx), abs(coef(fitEx)))
neCoef = data.frame(coef(fitNe), abs(coef(fitNe)))
exCoef = exCoef[order(-exCoef[,2]),]
neCoef = neCoef[order(-neCoef[,2]),]
print(exCoef[1:10, 1])
print(neCoef[1:10, 1])

# SQL Queries

# SELECT faculty_id, faculty_name FROM COURSES WHERE course_name="Name of Course"
# work on this

# WHERE operates on individual rows (so should be used before a GROUP BY call), whereas HAVING operates on aggregates (so should come after a GROUP BY call)

# given a join between table A and table B, a LEFT JOIN gives you all of table A with just the bits of table B added in that match the specified column(s) onto table A. A RIGHT JOIN gives you all of table B, with just the parts of table A which match the speficified column(s). An INNER JOIN gives you the intersection of the tables, or in other words, just the elements of the tables that match each other, ignoring any non-matching rows from each table.
