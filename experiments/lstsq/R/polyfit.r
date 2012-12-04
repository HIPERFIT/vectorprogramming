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

qrLeastSquare <- function(a,b) {
  qri <- qr(a)
  q <- qr.Q(qri)
  r <- qr.R(qri)

  x <- solve(t(r) %*% r, t(r) %*% t(q) %*% b)
  return(x)
}

vander <- function(x, deg) {
  a <- matrix(0, nrow=length(x), ncol=deg)
  for(i in 1:length(x)) {
    for(j in 0:deg-1) {
      a[i,j+1] <- x[i] ** j
    }
  }
  return(a)
}

polyfit <- function(x, y, degree, method = "Cholesky") {
  method <- match.arg(method, c("QR", "Cholesky"))

  a <- vander(x, degree)
  
  if(method == "Cholesky") {
    return(choleskyLeastSquare(a,y))
  } else if (method == "QR") {
    return(qrLeastSquare(a,y))
  }
}



testCholeskyLS <- function() {
  cat("Testing Cholesky Least Squares\n")
  # Example from Wikipedia
  # http://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)
  a <- matrix(c(1,1,1,2,1,3,1,4), nrow=4, byrow=TRUE)
  b <- c(6,5,7,10)
  x <- choleskyLeastSquare(a,b)

  # Expected output: x = [3,5; 1.4]
  print(x)
  cat("\n")

  # Example from Rolfs R code using 10 paths  
  a <- matrix(c(1, 99.61301, 9922.752,
                1, 99.80255, 9960.549,
                1, 99.98577, 9997.153,
                1, 98.59173, 9720.330,
                1, 99.21689, 9843.991,
                1, 99.15269, 9831.256,
                1, 99.40073, 9880.506,
                1, 99.81001, 9962.038,
                1, 98.43654, 9689.752,
                1, 99.51046, 9902.331), nrow=10, byrow=TRUE)
  b <- c(3.282227, 10.526936, 4.280280, 28.252172, 8.972325,
         22.156721, 0.000000, 5.778529, 2.179044, 3.302575)

  # Expected output: x =  -42911.361230    872.818480     -4.436812
  x <- choleskyLeastSquare(a,b)
  print(x)

  
  # Example from Rolfs R code using 10 paths
  a <- matrix(c(1, 98.33540, 9669.850,
                1, 97.52770, 9511.652,
                1, 99.31874, 9864.213,
                1, 98.52922, 9708.007,
                1, 99.35927, 9872.265,
                1, 99.16612, 9833.920,
                1, 99.59040, 9918.247,
                1, 98.92713, 9786.577,
                1, 99.46054, 9892.399,
                1, 99.16377, 9833.454), nrow=10, byrow=TRUE)
  b <- c(10.1477988, 3.9363954, 3.7511167, 15.4064057, 37.7197282, 0.3352728, 0.0000000, 2.7263035, 29.8441760, 19.1195146)

  # Expected output: x = -7512.6874468   147.3118192    -0.7201638 
  x <- choleskyLeastSquare(a,b)
  print(x)
}

testQRLS <- function () {
  cat("Testing QR Least Squares\n")
  # Example from Wikipedia
  # http://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)
  a <- matrix(c(1,1,1,2,1,3,1,4), nrow=4, byrow=TRUE)
  b <- c(6,5,7,10)
  x <- qrLeastSquare(a,b)
  
  # Expected output: x = [3,5; 1.4]
  print(x)
  cat("\n")

  ## # Example from Rolfs R code using 10 paths  
  ## a <- matrix(c(1, 99.61301, 9922.752,
  ##               1, 99.80255, 9960.549,
  ##               1, 99.98577, 9997.153,
  ##               1, 98.59173, 9720.330,
  ##               1, 99.21689, 9843.991,
  ##               1, 99.15269, 9831.256,
  ##               1, 99.40073, 9880.506,
  ##               1, 99.81001, 9962.038,
  ##               1, 98.43654, 9689.752,
  ##               1, 99.51046, 9902.331), nrow=10, byrow=TRUE)
  ## b <- c(3.282227, 10.526936, 4.280280, 28.252172, 8.972325,
  ##        22.156721, 0.000000, 5.778529, 2.179044, 3.302575)

  ## # Expected output: x =  -42911.361230    872.818480     -4.436812
  ## x <- qrLeastSquare(a,b)
  ## print(x)

  
  ## # Example from Rolfs R code using 10 paths
  ## a <- matrix(c(1, 98.33540, 9669.850,
  ##               1, 97.52770, 9511.652,
  ##               1, 99.31874, 9864.213,
  ##               1, 98.52922, 9708.007,
  ##               1, 99.35927, 9872.265,
  ##               1, 99.16612, 9833.920,
  ##               1, 99.59040, 9918.247,
  ##               1, 98.92713, 9786.577,
  ##               1, 99.46054, 9892.399,
  ##               1, 99.16377, 9833.454), nrow=10, byrow=TRUE)
  ## b <- c(10.1477988, 3.9363954, 3.7511167, 15.4064057, 37.7197282, 0.3352728, 0.0000000, 2.7263035, 29.8441760, 19.1195146)

  ## # Expected output: x = -7512.6874468   147.3118192    -0.7201638 
  ## x <- qrLeastSquare(a,b)
  
}

testLSFIT <- function () {
  cat("Testing R-builtin lsfit\n")
  # Example from Wikipedia
  # http://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)
  a <- matrix(c(1,1,1,2,1,3,1,4), nrow=4, byrow=TRUE)
  b <- c(6,5,7,10)
  x <- lsfit(a,b)

  # Expected output: x = [3,5; 1.4]
  print(x$coefficients)
  cat("\n")

  a <- matrix(c(99.61301, 9922.752,
                99.80255, 9960.549,
                99.98577, 9997.153,
                98.59173, 9720.330,
                99.21689, 9843.991,
                99.15269, 9831.256,
                99.40073, 9880.506,
                99.81001, 9962.038,
                98.43654, 9689.752,
                99.51046, 9902.331), nrow=10, byrow=TRUE)
  b <- c(3.282227, 10.526936, 4.280280, 28.252172, 8.972325,
         22.156721, 0.000000, 5.778529, 2.179044, 3.302575)

  # Expected output: x =  -42911.361230    872.818480     -4.436812
  x <- lsfit(a,b,intercept=T)
  print(x$coefficients)

  a <- matrix(c(98.33540, 9669.850,
                97.52770, 9511.652,
                99.31874, 9864.213,
                98.52922, 9708.007,
                99.35927, 9872.265,
                99.16612, 9833.920,
                99.59040, 9918.247,
                98.92713, 9786.577,
                99.46054, 9892.399,
                99.16377, 9833.454), nrow=10, byrow=TRUE)
  b <- c(10.1477988, 3.9363954, 3.7511167, 15.4064057, 37.7197282, 0.3352728, 0.0000000, 2.7263035, 29.8441760, 19.1195146)

  # Expected output: x = -7512.6874468   147.3118192    -0.7201638 
  x <- lsfit(a,b,intercept=T)
  print(x$coefficients)

}

testCholeskyLS()
testQRLS()
testLSFIT()

