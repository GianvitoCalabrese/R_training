#create original data frame
data <- data.frame(person=c('A', 'A', 'B', 'B', 'C', 'C'),
                   trial=c(1, 2, 1, 2, 1, 2),
                   outcome1=c(7, 6, 6, 5, 4, 4),
                   outcome2=c(4, 4, 5, 5, 3, 2))

#stack the third and fourth columns
stacked<-cbind(data[1:2], stack(data[3:4]))
View(stacked)