# Create a sequence of numbers between -10 and 10 incrementing by 0.1.
x <- seq(-10, 10, by = .1)

y<-rnorm(100, mean = 2.5, sd = 0.5)

# Choose the mean as 2.5 and standard deviation as 0.5.
#y <- dnorm(x, mean = 2.5, sd = 0.5)

print(length(y))

# Give the chart file a name.
png(file = "dnorm.png")

plot(x,y)


# R < scriptName.R --no-save  