library(data.table)
library(curl)
library(dplyr)
#dat = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")
#dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t')

dat = read.table("C:/Users/tele1/Downloads/Bin_Map_leader.txt", header=TRUE,  sep = '\t', skipNul=TRUE)
print(dat)
#dat

companies <- c("AAA","BBB","CCC")

param <- menu(colnames(dat), graphics=TRUE, title="Choose parameter")

i=0
x = dat[,c(param)]
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

