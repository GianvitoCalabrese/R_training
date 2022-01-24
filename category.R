library(dplyr)

data(beavers)
dataFrame <- beaver1

fltr<- filter(dataFrame, dataFrame$temp <quantile(dataFrame$temp, 0.25))


fltr <- fltr %>% mutate(GR = dplyr::case_when(fltr$time <= mean(fltr$time, na.rm=FALSE) ~ 'A'
          ,fltr$time <= mean(fltr$time, na.rm=FALSE) + sd(fltr$time) ~ 'B'
          ,fltr$time <= mean(fltr$time, na.rm=FALSE) + 2*sd(fltr$time)  ~ 'C'
          ,fltr$time <= mean(fltr$time, na.rm=FALSE) + 3*sd(fltr$time) ~ 'D'
          ,TRUE ~ 'F'))

print(fltr)