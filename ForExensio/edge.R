library(dplyr)
library(tidyr)
library(ClusterR)

dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)
resp = read.table("https://www.dropbox.com/s/hhws0dedltkgy9t/Bin%20Map%20wf%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)

dat$edge <-0

for(y in unique(dat$diey)){
    y_filterdat <- dat %>% filter(diey==y)
    for(i in 1:nrow(dat)){
        
        if( dat$diex[i] == max(y_filterdat$diex) || dat$diex[i] == min(y_filterdat$diex)){
            dat$edge[i] <- "1DIN"
        } 
    }
}

print(dat)
