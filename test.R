source("package.R")

# Number of points
n <- 10
# Number of measurements per subgroup
every = 15

var <- n- n%%every
dist <- rnorm(var)
i<-0
x<-c()
while(i < var/every){
lim1 <- 1+i*every
lim2 <- every*(i+1)
print(lim1)
print(lim2)
i <- i+1
x[i] <- c(mean(dist[lim1:lim2], na.rm = FALSE))
}

print(var)
print(x)
