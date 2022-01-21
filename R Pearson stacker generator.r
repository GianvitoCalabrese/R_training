library(data.table)
library(curl)
library(dplyr)
#dat = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")
dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)


param_set <- function(x) {        # Create user-defined function
 
  out <- menu(colnames(x), graphics=TRUE, title="Choose parameter")
 
  out
}


param<- colnames(dat)[param_set(dat)]
print(param)


i=0
x = dat[,c(param)]
typeof(x)
nums <- unlist(sapply(dat, is.numeric))
vec <- names(dat[, nums])


for(i in vec) {
print(i)
}

#a <- air_time

output <- data.table(Param = factor(), Pearson = numeric())
for(i in vec) {
print(typeof(i))
m <- lm(formula = get(i) ~ x, data=dat)
s <- summary(m)
new.row <- data.frame(Param = i ,Pearson = s$r.squared )
output <- rbind(output, new.row)
}

#warnings()
output

