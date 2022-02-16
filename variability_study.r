# Create a sequence of numbers between -10 and 10 incrementing by 0.1.

runif(10, min = -1, max = 1)

data <-rnorm(100, mean = 2.5, sd = 0.5)
hist(data)
y <- MASS::fitdistr(data, "normal")
# Choose the mean as 2.5 and standard deviation as 0.5.
#y <- dnorm(x, mean = 2.5, sd = 0.5)

print(y)
print(y[["estimate"]][["mean"]])
# Give the chart file a name.
png(file = "dnorm.png")

# R < scriptName.R --no-save  