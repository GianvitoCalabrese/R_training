#library(factoextra)

data("USArrests")      # Loading the data set
df <- na.omit(scale(USArrests)) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)
