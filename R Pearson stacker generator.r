library(data.table)
library(curl)
library(dplyr)

file_location <- file.choose()
path_user <- dirname(file_location)
print(path_user)
dat = read.csv(file_location, fileEncoding="UTF-16LE", header=TRUE,  sep = '\t', skipNul=TRUE)
source("C:/Users/gcalabre/GitHub/R_training/package.R")

print(colnames(dat))

param<- colnames(dat)[param_set(dat)]
print(param)
print(dat[,c(param)])
print(is.numeric(dat[,c(param)]))
print(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", dat[,c(param)]))


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

