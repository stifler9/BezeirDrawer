decasteljau_matr = function(b, t){
  n = dim(b)[2]
  if(n == 1){
    return(b)
  }
  dim = dim(b)[1]
  rez = list()
  for (i in 1:dim) {
    tempm = matrix(nrow = n, ncol = n)
    tempm[1,] = b[i,]
    for(r in 2:n){
      for(j in 1:(n-r+1)){
        tempm[r,j] = (1-t)*tempm[r-1,j] + t*tempm[r-1,j+1]
      }
    }
    rez[[i]] = tempm
  }
  return(rez)
}

decasteljau_p = function(b, t){
  dim = dim(b)[1]
  n = dim(b)[2]
  rez = matrix(nrow = dim, ncol = 1)
  for (i in 1:dim) {
    rez[i,1] = decasteljau_matr(b, t)[[i]][n, 1]
  }
  return(rez)
}

decasteljau = function(b, t_series){
  tl = length(t_series)
  rez = matrix(nrow = dim(b)[1], ncol = tl)
  for(x in 1:tl){
    rez[,x] = decasteljau_p(b, t_series[x])
  }
  return(rez)
}