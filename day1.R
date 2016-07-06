1*3
x = 100
x
typeof(4)
typeof(4.3)
typeof(x)
typeof('hello')
typeof(TRUE)
typeof(integer(4))

b = typeof(integer(5.5L))
b = integer(5.5L)
typeof(b)
is.numeric(b)
is.numeric(5L)
is.numeric('c')
TRUE == FALSE
TRUE & (T | FALSE)
F = T
F & TRUE

g = c()
is.numeric(g)

is.atomic(g)
v = c(1, c(2, 3))
v = c(v, 4)
v = c(v, 'b', FALSE)
v
v2 = c(TRUE, 1, 0, FALSE)
v2
as.logical(v2)

sum(v2)
mean(v2)
length(v2)
mean(v2) * length(v2)
c(1, FALSE)
c("a", 1)
c(list(1, "b"), "a")

x = runif(1)
x
if (x< 0.5) {
  print(x)
} else {
  print('big')
}

lbl = c()
for (i in 1:30) {
lbl = c(lbl, paste("label", i, sep=" ") )
lbl
        }
lbl

for(i in rnorm(10)) {
  if(i < 0.5){
    print(i)
  } else {
    print("big")
  }
}

func1 = c()
for(i in 10:100){
  func1 = c(func1, (i^3 + 4*i^2))
}
sum(func1)

func2 = c()
for(i in 1:25){
  func2 = c(func2, ((2^i)/i + (3^i)/i^2))
}
sum(func2)


times = c()
for(i in 1:1000) {
startTime = proc.time()
a = x[(length(x)-5):length(x)]
totalTime = proc.time() - startTime
times = c(times, totalTime[3])
}
mean(times)
times = c()
for(i in 1:1000) {
  startTime = proc.time()
  a = x[5:10]
  totalTime = proc.time() - startTime
  times = c(times, totalTime[3])
}
mean(times)
times = c()
for(i in 1:1000) {
  startTime = proc.time()
  a = tail(x)
  totalTime = proc.time() - startTime
  times = c(times, totalTime[3])
}
mean(times)

ecos = c()
for(i in 30:60) {
  ecos = c(ecos, exp(i/10) * cos(i/10))
}
ecos

for(i in 1:20){
  for(j in i:20){
    print(c(i,j))
  }
}

collatz = function(n) {
  if(n%%2 == 0){
    return(n/2)
  } else {
    return(3*n+1)
    }
}
collatz_memo_list = c()
for(i in 200){
  collatz_memo_list = c(collatz_memo_list, collatz(i))
}
collatz_memo = function(n) {
  if(length(collatz_memo_list)<=n){
    return(collatz_memo_list[n])
  } else {
    return(collatz(n))
  }
}

times = c()
for(i in 1:1000) {
  startTime = proc.time()
  collatz(150)
  totalTime = proc.time() - startTime
  times = c(times, totalTime[3])
}
mean(times)

times = c()
for(i in 1:1000) {
  startTime = proc.time()
  collatz_memo(150)
  totalTime = proc.time() - startTime
  times = c(times, totalTime[3])
}
mean(times)

collatz(5)
collatz(2)
collatz(collatz(collatz(collatz(collatz(collatz(3))))))
cltzList = c()
for(i in 1:100){
  print(i)
  j = collatz(i)
  print(j)
  k = collatz(j)
  print(k)
  count = 2
  reps = c(i, j)
  while(k %in% reps == FALSE){
    reps = c(reps, k)
    k = collatz(k)
    count = count + 1
  }
  print(count)
  cltzList = c(cltzList, count)
}
summary(cltzList)
hist(cltzList)



cubes=c()
for(i in 1:2000){
  cubes = c(cubes, i^3)
}
harRam = function(n){
  ways = 0
  cubeOne = 0
  cubeTwo = 0
  cubesTried = c()
  topPossible = round(n^(1/3))
  for(i in 1:topPossible){
    cubeOne = cubes[i]
    for(j in 1:topPossible){
      cubeTwo = cubes[j]
      if(cubeOne + cubeTwo == n){
        if(cubeOne %in% cubesTried == FALSE){
          ways = ways + 1
          cubesTried = c(cubesTried, cubeOne, cubeTwo)
          if(ways>1){
            print(c("Way", ways, "n = ", n, "Cube One = ", cubeOne, "Cube Two = ", cubeTwo))
          }
        }
      }
    }
  }
  return(ways)
}
harRam(840)

for(i in 1:190000){
  if(harRam(i)>1){
    print(i)
    break
    }
  }

fib = function(n){
  if(n<1){
   print("Error. n too small")
   return(0)
  }
  if(n==1){
    return(1)
  }
  if(n==2){
    return(2)
  }
  fibn = 1
  oneBack = 1
  for(i in 2:n){
    twoBack = oneBack
    oneBack = fibn
    fibn = twoBack + oneBack
  }
  return(fibn)
}

fib_test = function(n, k){
  if(n > 0){
    for(i in 1:n){
      if(fib(i)%%k==0){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

times = c()
for(i in 1:1000) {
  startTime = proc.time()
  fib_test(40, 27)
  totalTime = proc.time() - startTime
  times = c(times, totalTime[3])
}
mean(times)


fib_seq = c()
for(i in 1:1000){
    fib_seq = c(fib_seq, fib(i))
}

fib_test_memo = function(n, k){
  if(n > 0){
    for(i in 1:n){
      if(fib_seq[i] %% k == 0){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

times = c()
for(i in 1:1000) {
  startTime = proc.time()
  fib_test_memo(40, 27)
  totalTime = proc.time() - startTime
  times = c(times, totalTime[3])
}
mean(times)

