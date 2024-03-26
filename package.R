library(dplyr)
library(tidyr)

# Define the wafer class
setClass("wafer", representation(dat = "data.frame"))

# Constructor function for the wafer class
wafer <- function(dat) {
  new("wafer", dat = dat)
}

# Method to detect edge
detect_edge <- function(z, diex, diey) {
  edge <- rep(0, nrow(z@dat))
  for(y in unique(z@dat[[diey]])){
    y_filterdat <- z@dat[z@dat[[diey]] == y, ]
    for(i in 1:nrow(z@dat)){
      if( z@dat[i,diey] == y & (z@dat[i,diex] == max(y_filterdat[[diex]]) | z@dat[i,diex] == min(y_filterdat[[diex]]))){
        edge[i] <- "1DIN"
      } 
    }
  }
  
  
  for(x in unique(z@dat[[diex]])){
    print(x)
    x_filterdat <- z@dat[z@dat[[diex]] == x, ]
    print(x_filterdat)
    for(i in 1:nrow(z@dat)){
      if( z@dat[i,diex] == x & (z@dat[i,diey] == max(x_filterdat[[diey]]) | z@dat[i,diey] == min(x_filterdat[[diey]]))){
        edge[i] <- "1DIN"
      } 
    }
  }
  
  z@dat$edge <- edge
  return(z@dat)
}

# Sample data
lot_id <- c("abcd", "abcd", "abcd", "abcd", "abcd", "abcd", "abcd", "abcd", "abcd")
wf_id <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
first_column <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
second_column <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
tab <- data.frame(lot_id, wf_id, first_column, second_column)

# Create a wafer object
wf1 <- wafer(tab)

# Call the detect_edge function
detect_edge(wf1, "first_column", "second_column")



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

setClass("random_walk", contains= "numeric", representation(k= "numeric") )
setGeneric("generate", function(x, N, x0, mu, variance) standardGeneric("generate"))
setMethod("generate" , "random_walk" ,function(x, N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x@k<-x0+t*mu+z
  x
}) 

john <- new("random_walk", k = NA_real_)
slot(john, "k")
generate(john, 50,0,3,0.2)
  