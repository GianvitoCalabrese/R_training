# Create a sequence of numbers between -10 and 10 incrementing by 0.1.

runif(10, min = -1, max = 1)
vec_m <- 0
vec_s <- 0
for(i in 1:500){
data <-rnorm(i*100, mean = 2.5, sd = 1)
#hist(data)
vec_m[i] <- MASS::fitdistr(data, "normal")[["estimate"]][["mean"]]
vec_s[i] <- MASS::fitdistr(data, "normal")[["estimate"]][["sd"]]
# Choose the mean as 2.5 and standard deviation as 0.5.
#y <- dnorm(x, mean = 2.5, sd = 0.5)
}
plot(vec_s)
#print(y[["estimate"]][["mean"]])
# Give the chart file a name.
png(file = "dnorm.png")

# R < scriptName.R --no-save  