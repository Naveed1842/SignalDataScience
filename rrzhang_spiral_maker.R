## Given n*m, create a clockwise or counterclockwork list of coordinates
cw = list(c(0,1),c(1,0),c(0,-1),c(-1,0))
ccw = list(c(1,0), c(0,1), c(-1,0), c(0,-1))

spiral_maker = function(n,m,ccw=FALSE){
  coords = list(c(1,1))
  l = length(coords)
  d = 0
  while (l<(n*m)){
    next_sp = next_spiral(coords,d,ccw,n,m)
    next_cor = next_sp[[1]]
    d = next_sp[[2]]
    coords = c(coords,next_cor)
  }
  
  return (coords)
}

next_spiral = function(coords,d,ccw,n,m){
  di = NULL
  if (ccw==FALSE){
    di = cw[[d%%4+1]]
  } else {
    di = ccw[[d%%4+1]]
  }
  l = length(coords)
  cor = coords[[l]]
  next_cor = c(cor + di)
  print("next_cor")
  print(next_cor)
  print(d)
  in_prev = any(next_cor%in%coords)
  out_border = (next_cor[1]>n | next_cor[2]>m | next_cor[1]<=0 | next_cor[2]<=0)
  print(out_border)
  viable = in_prev|out_border
  if  (viable){
    return(list(coords, d%%4+2))
  } else {
    return(list(c(coords,list(next_cor)),d%%4+1))
  }
  
}

spiral_maker(3,3)
temp_cor = list(c(1,1),c(1,2))
next_spiral(temp_cor, 1, FALSE, 3,3)



test = c(0:20)
test2 = test%%4+1
