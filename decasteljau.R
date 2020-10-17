# matrix of decasteljau algorithm
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

# curve from t=0 to t = tplus
curveplus = function(b, tplus){
  dim = dim(b)[1]
  n = dim(b)[2]
  matr = decasteljau_matr(b, tplus)
  rez = matrix(nrow = dim, ncol = n)
  for(i in 1:dim){
    rez[i,] = matr[[i]][,1]
  }
  return(rez)
}

# curve from t=tminus to t = 1
curveminus = function(b, tminus){
  dim = dim(b)[1]
  n = dim(b)[2]
  matr = decasteljau_matr(b, tminus)
  rez = matrix(nrow = dim, ncol = n)
  for(i in 1:dim){
    for(j in 1:n) {
      rez[i,j] = matr[[i]][(n-j+1),j]
    }
  }
  return(rez)
}

# split curve to 2 with same amount of points
splitcurve = function(b, tmid){
  rez = list()
  rez[[1]] = curveplus(b, tmid)
  rez[[2]] = curveminus(b, tmid)
  return(rez)
}
