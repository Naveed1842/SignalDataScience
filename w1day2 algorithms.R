arg_max <- function(v){
  maximum = max(v)
  matches = match(maximum, v)
  print(matches[1])
}

v=c(1,2,3,6,4)
arg_max(v)


longest_run <- function(v){
  runs = rle(v)
  max_length = max(runs$lengths)
  match_to_value = match(max_length, runs$lengths)
  print(c('Value: ', runs$values[match_to_value], 'Length: ', runs$lengths[match_to_value]))
}

v = c(1,1,1,1,1,1,1,4)
longest_run(v)



resevoir <- function(v,k){
  k_items = v[1:k]
  for (i in (k+1):(length(v))){
    random_int = floor(runif(1,1,i))
    if (random_int <= k){
      k_items[random_int] = v[i]
    }
  }
  return(k_items)
}

v = c(1:20)
r=resevoir(v, k=5)

run_resevoir <- function(iterations, v, k){
  totalResults = c()
  for (i in 1:iterations){
    r = resevoir(v,k)
    totalResults = c(totalResults, r)
  }
  nums = table(totalResults)
  print(nums)
  for (i in 1:(length(nums))){
    print(nums[i])
    probability = nums[i]/length(totalResults)
    print(c('Probability of ', names(nums)[i], "is", probability))
  }
  #number of occurances in totalResults / length(totalResults)

}

v = c(1:20)
run_resevoir(50000, v, k=5)


perm_naive <- function(n){
  if (n==1){
    return(list(1))
  }else{
    list_of_vectors = list(1:n)
    for (k in 1:n){
      for (i in 1:(n-1)){
        list_each = 1:n
        list_each[i] = k
        for(j in (i+1):k){
         list_each[j] = (j-1)
        }
        #print(list_each)
        list_of_vectors = list(list_of_vectors, list_each)
      }
    }
    print(list_of_vectors)
  }
}

perm_naive <- function(n){
  list_of_perms = list()
  if (n==1){
    return(list(1))
  }else{
    for (elem in perm_naive(n-1)){
      for (i in 1:(n-1)){
        list_after_i = elem[i:length(elem)]
        elem[i] = n
        elem[i+1:(length(elem)+1)] = list_after_i
        print(elem)
        list_of_perms = list(list_of_perms, elem)
      }
    }
  }
  return(list_of_perms)
}

perm_naive(3)

start = proc.time()
# Permutation generation (naive) solution
perm_naive = function(n) {
  if (n == 1) {
    return(list(1))
  }

  smaller = perm_naive(n-1)
  perms = vector("list", length(smaller) * n)
  pos = 0
  for (i in 1:length(smaller)) {
    old = smaller[[i]]
    for (j in 1:(n-1)) {
      pos = pos + 1
      if (j < (n-1)) {
        perms[[pos]] = c(old[1:j], n, old[(j+1):(n-1)])
      } else {
        perms[[pos]] = c(old, n)
      }
    }
    pos = pos + 1
    perms[[pos]] = c(n, old)
  }
  unique(perms)
}
perm_naive(8)

done = proc.time() - start
print(done)

perm_lexicon <- function(n){
  if (typeof(n) == 'double'){
    a = 1:n
    print(a)
    #list_of_perms = list()
  }else{
    a = n
  }
  k = 0
  l = 0
  for (i in 1:(length(a)-1)){
    if (a[i]<a[i+1]){
      k = i
    }
  }
  if (k == 0){
    return(NULL)
  }
    for (j in k:length(a)){
      if (a[k]<a[j]){
        l = j
      }
    }
  swap = a[k]
  a[k] = a[l]
  a[l] = swap
  a = c(a[1:k], rev(a[(k+1):length(a)]))
  print(a)
  #list_of_perms = list(list_of_perms, a)
  perm_lexicon(a)
}

perm_lexicon(3)


qs <- function(v){
  if (length(v)==0){
    return(NULL)
  }
  if (length(v)==1){
    return(v)
  }else{
    i = floor(runif(1,1,length(v)))
    lesser = c()
    greater = c()
    for (j in 1:length(v)){
      if (v[j] < v[i]){
        lesser = c(lesser, v[j])
      }
      if (j!=i && v[j] >= v[i]){
        greater = c(greater, v[j])
      }
    }
  }
  return(c(qs(lesser), v[i], qs(greater)))
}

print(qs(c(1,2,4,3,2,6)))

starttime = proc.time()
for (i in 1:100){
  v = sample(1:100, 10, replace = TRUE)
  sort(v)
}
totaltime = proc.time() - starttime
print(totaltime)

quickselect<- function(L,k){
  if (length(L)==0){
    return(NULL)
  }
  if (length(L)==1){
    return(L)
  }else{
    i = floor(runif(1,1,length(L)))
    lesser = c()
    greater=c()
    for (j in 1:length(L)){
      if (L[j] < L[i]){
        lesser = c(lesser, L[j])
      }
      if (L[j] >= L[i]){
        greater = c(greater, L[j])
      }
      if (i==j && length(L)==2){
        return(max(L))
      }
    }
    if (length(lesser) < k){
      k = k-length(lesser)
      quickselect(greater, k)
    }else{
      quickselect(lesser,k)
    }
  }
}

print(quickselect(c(4,5,3,4,1,2,7,18,31,51,3,80), k=5))
