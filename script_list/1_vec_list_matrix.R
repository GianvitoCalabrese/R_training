#c means concatenate
v1 <- c(1,2,3,4,5)
v2 <- c(1,2)
class(v1)
typeof(v1)
v1[2]
v1
x<- vector('numeric',10)
x

v3 <- c(v1,!v2)
v3

#vectors always cast all the mixed objects, coercion occurs. In the list different objects can coexist.

x<- 0:6
as.numeric(x)
as.logical(x)

#attributes
x<-1
attributes(x)

#list -> can contain all kind of R object
test <- c('music tracks', 100, 5)
test
test <- list('music tracks', 100, 5)
test
test[3]
test[[3]]
is.list(test)
names(test) <- c('product','price','stars')
prod.category <- list(product='music tracks', count=100, ratings=5)
prod.category
str(prod.category)
attributes(prod.category)
test
prod.sim <- list(product='eggs', count=70, ratings=3)
prod.category <- list(product='music tracks', count=100, ratings=5,prod.sim)
str(prod.category)
prod.category[["product"]]
prod.category[as.logical(c(1,1,1,0))]

#matrix -> matrix is 2 dimensional and is casting the values exactly like the vectors
matrix(1:6, nrow=2)
matrix(c(1,3,4,5), nrow=2)
#by default it fullfill bycolumn...
matrix(1:6, nrow=2,byrow=TRUE)
#repating till fullfill...
matrix(1:3, nrow=2,ncol=3)
#error message input length greeater than the size ...
matrix(1:12, nrow=2,ncol=3)

rbind(1:4, c(1,3,4,5))
cbind(1:4, c(1,3,4,5))

n<-matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
rbind(n, 7:9)
cbind(n,c(9,18))
