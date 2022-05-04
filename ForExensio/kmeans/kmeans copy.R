set.seed(2)
f.data=data.frame("Group"=c(rep("G1",25),rep("G2",25)),
"X1"=c(rnorm(25)+3,rnorm(25)), "X2"=c(rnorm(25)-4,rnorm(25)))

kmeans(x, centers, iter.max = 10, nstart = 1,
algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
"MacQueen"), trace=FALSE)


