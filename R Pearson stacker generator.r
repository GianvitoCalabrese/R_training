library(data.table)
library(curl)
library(dplyr)
dat = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")

#dat
i=0
x = dat[,c('air_time')]
typeof(x)
nums <- unlist(sapply(dat, is.numeric))
vec <- names(dat[, ..nums])

vec

for(i in vec) {
print(i)
}

a <- air_time
print(typeof(a))

output <- data.table(Param = factor(), Pearson = numeric())
for(i in vec) {
print(typeof(i))
m <- lm(formula = get(i) ~ air_time, data=dat)
s <- summary(m)
new.row <- data.frame(Param = i ,Pearson = s$r.squared )
output <- rbind(output, new.row)
}

#warnings()
output

