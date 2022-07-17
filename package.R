library(dplyr)
library(tidyr)



BinMean <- function (n, every, na.rm = FALSE) {
       var <- n- n%%every
       print(n)
       print(n%%every)
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
  
  
edge <- function(dat){
  
  dat$edge <-0
  
  for(y in unique(dat$diey)){
    y_filterdat <- dat %>% filter(diey==y)
    for(i in 1:nrow(dat)){
      
      if( dat$diey[i] == y & (dat$diex[i] == max(y_filterdat$diex) | dat$diex[i] == min(y_filterdat$diex))){
        dat$edge[i] <- "1DIN"
      } 
    }
  }
  
  
  for(x in unique(dat$diex)){
    x_filterdat <- dat %>% filter(diex==x)
    for(i in 1:nrow(dat)){
      
      if( dat$diex[i] == x & (dat$diey[i] == max(x_filterdat$diey) | dat$diey[i] == min(x_filterdat$diey))){
        dat$edge[i] <- "1DIN"
      } 
    }
  }
  
  dat
}
  