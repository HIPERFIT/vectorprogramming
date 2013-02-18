## Least squares fit of x in Ax = b
choleskyLeastSquare <- function(a,b) {
  aT <- t(a)

  c <- aT %*% a
  d <- aT %*% b
  g <- chol(c)
  gT <- t(g)

  y <- solve(gT, d)
  x <- solve(g, y)
  return(x)
}

vander <- function(x, deg) {
  a <- matrix(1, nrow=length(x), ncol=deg)
  for(j in 0:deg-1) {
      a[,j+1] <- x ^ j
  }
  
  return(a)
}

polyfit <- function(x, y, degree) {
  a <- vander(x, degree)
  return(choleskyLeastSquare(a,y))
}
