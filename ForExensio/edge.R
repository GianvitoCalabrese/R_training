library(dplyr)
library(tidyr)
library(ClusterR)

dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)
resp = read.table("https://www.dropbox.com/s/hhws0dedltkgy9t/Bin%20Map%20wf%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)

grp_df <- dplyr::filter(dat, grepl("Id",BinName)) %>% mutate(edge = case_when(
    for(y in dat$diey){  if( dat$diex == max(dat[,diey=y]$diex) || dat$diex == min(dat[,diey=y]$diex)){} } ~ "1DIN",
    for(x in  dat$diex){  if(dat$diey == max(dat[,diex=x]$diey) || dat$diey == min(dat[,diex=x]$diey)){} }   ~ "1DIN"
    ) %>% count(wafer, edge, sort = TRUE)