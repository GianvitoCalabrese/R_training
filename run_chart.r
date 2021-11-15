## Generate a random sample of 100 numbers from a normal distribution
## with mean 10 and standard deviation of 2
# Given information (but calculate yourself
# from https://openmv.net/info/rubber-colour)
xbar = c(245, 239, 239, 241, 241, 241, 238,
         238, 236, 248, 233, 236, 246, 253,
         227, 231, 237, 228, 239, 240)

# Number of measurements per subgroup
N.sub = 5

# Average of the 20 standard deviations
# of the 20 subgroups
S = 9.28

# xdb = x double bar = overall mean =
#       mean of the means
xdb = mean(xbar)

num.an = sqrt(2) * gamma(N.sub/2)
den.an = sqrt(N.sub-1) * gamma((N.sub-1)/2)
an = num.an / den.an

LCL = xdb - (3 * S/(an * sqrt(N.sub)))
UCL = xdb + (3 * S/(an * sqrt(N.sub)))
paste0('Control limits: [', round(LCL, 2),
       '; ', round(UCL,2), ']')

paste0('Number > UCL: ', sum(xbar > UCL))
paste0('Number < LCL: ', sum(xbar < LCL))

# Exclude the one subgroup above the UCL.
# Do this by setting it to 'NA' (missing)
xbar[xbar > UCL] = NA

# Calculate the mean, removing missing
# values (ignore it).
xdb = mean(xbar, na.rm=TRUE)

# 'S' will change also. If you download the
# raw data (link above), you can prove
# that the new 'S' will be:
S = 9.68

# The 'an' and 'N.sub' will not change.

LCL = xdb - (3 * S/(an * sqrt(N.sub)))
UCL = xdb + (3 * S/(an * sqrt(N.sub)))
paste0('Control limits: [', round(LCL, 0),
       '; ', round(UCL,0), ']')

plot(x = 1,
     type = "n",
     xlim = c(0, length(xbar)), 
     ylim = c(200, max(xbar, na.rm = TRUE)),
     pch = 16,
     xlab = "N", 
     ylab = "Values",
     main = "Run Chart")

points(x= 1:length(xbar), y = xbar, pch=16, col=ifelse(xbar>230, "red", "black"))
lines(x = 1:length(xbar) , y = xbar, type = "l")
