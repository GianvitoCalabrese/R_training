#library(factoextra)
file_location <- file.choose()
print(path_user)
#dat = read.csv(file_location, fileEncoding="UTF-16LE", header=TRUE,  sep = '\t', skipNul=TRUE)
dat = read.csv(file_location, header=TRUE,  sep = '\t', skipNul=TRUE)
data("USArrests")      # Loading the data set
source("C:/Users/tele1/OneDrive/Documenti/GitHub/R_training/package.R")


df <- na.omit(scale(USArrests)) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)

dat<- edge(dat)


plot(dat$diex, dat$diey,
     pch = 19,
     col = factor(dat$edge))