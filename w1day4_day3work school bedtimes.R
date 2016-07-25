

load("/Users/nathanhelm-burger/Documents/Dropbox/Stats Programming/Signal Data Science Nathan/time.dat")
# 42 is school bedtime, 43 is summer bedtime
str(df)
levels(df$H2GH42)
levels(df$H2GH43)
df$school = as.character(df$H2GH42)
df$summer = as.character(df$H2GH43)
df[df=="09:03P"]="09:30P"
df[df=="10:33P"]="10:30P"
df[df=="12:01A"]="12:00A"
df[df=="12:33A"]="12:33A"
df[df=="09:39P"]="09:30P"

df$summer=gsub("9999",NA,df$summer)
df$school=gsub("9999",NA,df$school)
unique(df$summer)

# df$summer = apply(df$summer,FUN=paste, c("M"))
df2 = na.omit(df[3:4])

for(j in 1:2){
  for(i in 1:length(df2[,j])){
     df2[i,j] = paste(df2[i,j], "M", sep="")
  }
}



df2$school.time = as.POSIXlt(df2$school, format="%I:%M%p")
df2$summer.time = as.POSIXlt(df2$summer, format="%I:%M%p")

eight = as.POSIXlt("8:00PM", format="%I:%M%p")

df2$sub.school.time = (df2$school.time - eight)/(60**2)
df2$sub.summer.time = (df2$summer.time - eight)/(60**2)
(24*60*60)
hist(as.numeric(df2$sub.summer.time),breaks=24)
an = df2$sub.school.time[as.numeric(df2$sub.school.time)<(-2)]
typeof(an)

df2$sub.school.time = as.double(df2$sub.school.time)
print(df2$sub.school.time[(df2$sub.school.time)<(-2)])
#dplyr::transmute(df2$sub.school.time[(df2$sub.school.time)<(-2)], sum(df2$sub.school.time,24.0))

#df2$sub.school.time[(df2$sub.school.time)<(-2)]= sum(df2$sub.school.time,24.0)

df2$sub.school.time[df2$sub.school.time<(-2)] = lapply(df2$sub.school.time[df2$sub.school.time<(-2)], function(x) x+24)

df2$sub.summer.time[df2$sub.summer.time<(-2)] = lapply(df2$sub.summer.time[df2$sub.summer.time<(-2)], function(x) x+24)

rev(dim(m))
attributes(m)$dim=c(3,10)
dim(m)
matrixchange = function(mat){
  v=rev(dim(mat))
  attributes(mat)$dim=v
  return(mat)
}
matrixchange(m)

df = data.frame(matrix(1:100, nrow=10))
df[5, 5] = NA
df[6, 6] = NA

isdiv = function(df,k){
  df = na.omit(df)
  return(unlist(df[df%%k == 0]))
}

isdiv(df,2)

min_matrix = function(n,m){
  v=c()
  for(i in 1:m){
    for(j in 1:n){
      v=c(v,min(i,j))
    }
  }
  matrix(v,nrow=n,ncol=m,byrow = TRUE)
}
m = min_matrix(3,3)
m==t(m)

trace = function(mat){
  return(sum(diag(mat)))
}

trace(m)

m2 = matrix(2,3,3)
trace(m+m2)
trace(m)+trace(m2)
