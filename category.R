library(dplyr)

data(beavers)
dataFrame <- beaver1

fltr<- filter(dataFrame, dataFrame$temp <quantile(dataFrame$temp, 0.25))


fltr <- fltr %>% mutate(GR = case_when(fltr$'time' <= mean(fltr$time, na.rm=TRUE) ~ 'A'
          ,fltr$time <= mean(fltr$time, na.rm=TRUE) + sd(fltr$time, , na.rm=TRUE) ~ 'B'
          ,fltr$time <= mean(fltr$time, na.rm=TRUE) + 2*sd(fltr$time, , na.rm=TRUE)  ~ 'C'
          ,fltr$time <= mean(fltr$time, na.rm=TRUE) + 3*sd(fltr$time,, na.rm=TRUE) ~ 'D'
          ,TRUE ~ 'F'))

print(mean(fltr$'time', na.rm=FALSE))