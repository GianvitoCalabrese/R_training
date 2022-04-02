BinMean <- function (n, every, na.rm = FALSE) {
       var <- n- n%%every
       dist <- rnorm(var)
       i<-0
       x<-c()
       while(i < var/every){
       lim1 <- 1+i*every
       lim2 <- every*(i+1)
       i <- i+1
       x[i] <- c(mean(dist[lim1:lim2], na.rm = FALSE))
       }
       x
       }


  RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
  }

  param_set <- function(x) {        # Create user-defined function
 
  out <- menu(colnames(x), graphics=TRUE, title="Choose parameter")
 
  out
}