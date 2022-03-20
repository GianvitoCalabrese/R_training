library(data.table)
library(curl)
library(dplyr)

#options(encoding = "UTF-8-BOM")
#dat = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")
dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", fileEncoding="UTF-16LE", header=TRUE,  sep = '\t', skipNul=TRUE)
source("package.R")

print(colnames(dat))

while(TRUE){
param<- colnames(dat)[param_set(dat)]
print(param)
print(dat[,c(param)])
print(is.numeric(dat[,c(param)]))
print(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", dat[,c(param)]))
if(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", dat[,c(param)])){
  break
}
}

i=0
x = dat[,c(param)]
typeof(x)
nums <- unlist(sapply(dat, is.numeric))
vec <- names(dat[, nums])


output <- data.table(Param = factor(), Pearson = numeric())
for(i in vec) {
m <- lm(formula = get(i) ~ x, data=dat)
s <- summary(m)
new.row <- data.frame(Param = i ,Pearson = s$r.squared )
output <- rbind(output, new.row)
}

#warnings()
print(output)

