mtcars
attributes(mtcars)$row.names[1] = "hi"
attributes(mtcars)[1]
attributes(mtcars)[2]
mtcars2 = mtcars
attr(mtcars2, "Mazda RX4")="hi"
mtcars2
attributes(mtcars)$row.names["hi"]
mtcars

name_doubler2 = function(df){
  for(j in 1:2){
    temp = c()
    for(i in attributes(df)[j]){
      temp = c(temp, paste(i,i))
      #print(typeof(i))
      #print(as.character(attr(df,i)))
      #v = c(v,i)
    }
    print(temp)
    attr(df, j) = temp
  }
  return(df)
}


mtcars2 = name_doubler2(mtcars)
mtcars2

v = c(1,3,5,2,5)
v2=factor(v)
levels(v2)
#undoes factoring
v2=c(v2,8)

v3=c(v2[1],v2[2])
v4=list(1,2,3,".")
v4=factor(v4)
v4=as.character(v4)

f1 = factor(letters)
f2 = rev(factor(letters))
f3 = factor(letters, levels = rev(letters))

f1
f2
f3
fruit = c("apple","grapefruit", "NA", "apple", "apple", "-", "grapefruit", "durian")

fruitfactor = factor(fruit, exclude = c("NA", "-"))
fruitfactor

df_col_to_factor = function (df) {
  tophalf = as.character(df[1:floor(ncol(df)/2)])
  tophalf = as.numeric(tophalf)
  return(factor(tophalf))
}
df_col_to_factor(df)


df_five_factor = function (df) {
  for(col in colnames(df)){
    if(length(unique(df[[col]]))<=5){
      df[[col]] = factor(df[[col]])
    }
  }
  return(df)
}

factormt = df_five_factor(mtcars)
toymatrix = matrix(data = sample(30, 100, replace = TRUE), nrow = 10, ncol = 5)
toymatrix[toymatrix<5] = NA
toydf = data.frame(toymatrix)



fill_na = function (df) {
  for(col in colnames(df)){
    common = names(sort(table(df[,col]), decreasing = TRUE)[1])
    print(common)
    #gsub(NA, common, df[col])

    #temp = as.character(df[,col])
    #temp[temp==NA] = common
    df[is.na(df[,col]),col] = common
   # print(temp)
   # df[,col]=temp
  }
  return(df)
}
toydf2 = fill_na(toydf)

sub1 = toydf[,2]
names(sort(table(sub1), decreasing = TRUE)[1])
subset18 =as.character(sub1[sub1=="NA"])

fill_na(toydf)

factormt[,is.factor(factormt)]
is.factor(factormt[,2])






binary_indicator = function(df){
  namesv=c()
  count=0
  binaryList = data.frame()
  binaryList[1:nrow(df),] = 0
  col_numb = length(colnames(df))
  for(col in 1:col_numb){
    if(is.factor(df[,col])){
      for(lev in 1:(length(levels(df[,col])))){
        temp_vector = c()
        check_col = as.numeric(df[,col])
        temp_level = levels(df[,col])[1]
        count=count+1
        namesv = c(namesv,(paste(names(df)[col], levels(df[,col])[lev], sep = "_")))
        temp_vector = check_col == lev
        binaryList = cbind(binaryList, temp_vector)
        print(c("count", count))
      }
    }
  }
  #binaryList = data.frame(binaryList[1:count])
  print(summary(binaryList))
  names(binaryList) = namesv
  df = cbind(df, binaryList)
  return(df)
}
bi = binary_indicator(factormt)
str(bi)


