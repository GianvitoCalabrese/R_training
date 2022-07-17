#library(factoextra)
dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)
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