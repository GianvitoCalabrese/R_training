BinMean <- function (n, every, na.rm = FALSE) {
  #set.seed(n) 
  dist <- rnorm(n)
  x <- .colMeans(dist, every, n %/% every, na.rm)
  r <- n %% every
  if (r) x <- c(x, mean.default(dist[(n - r + 1):n], na.rm = na.rm))
  x
  }