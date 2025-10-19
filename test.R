#library(factoextra)
#file_location <- file.choose()
#print(path_user)
#dat = read.csv(file_location, fileEncoding="UTF-16LE", header=TRUE,  sep = '\t', skipNul=TRUE)
#dat = read.csv(file_location, header=TRUE,  sep = '\t', skipNul=TRUE)
data("USArrests")      # Loading the data set
#source("C:/Users/gcalabre/GitHub/R_training/package.R")



df <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rlnorm(100, meanlog = 3, sdlog = 1), ncol = 2 ))
colnames(df) <- c("x", "y")

# View the firt 3 rows of the data
head(df, n = 3)

set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
plot(df, col = km.res$cluster)

#plot(dat$diex, dat$diey,
#     pch = 19,
#     col = factor(dat$edge))


#dat<- edge(dat)
#print(dat)