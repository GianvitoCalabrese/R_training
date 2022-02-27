# load data
#madeUp=read.table("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/madeUp.csv", sep=",", header=T)
dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)
# load libraries
library(tidyverse)
wfqty <- length(unique(dat$wafer))
print(wfqty)
# Prepare data
# X.Axis = diex, X.Axis = diey, Group can be Lot, wafers.. it's the trellis, randVals should be bin/ewfsqty
#theData <- madeUp %>% 
#group_by(X.Axis, Y.Axis, Group) %>% 
#dplyr::summarize(statistic=mean(randVals/wfqty, na.rm = TRUE))

theData <- dat %>% 
    group_by(diex, diey, BinName) %>% 
    dplyr::summarize(statistic=mean(length(BinName)/wfqty, na.rm = TRUE))



 
# plot
ggplot(theData, aes(diex, diey)) +
    
    coord_cartesian(xlim = c(min(dat$diex),max(dat$diex)), ylim = c(min(dat$diey),max(dat$diey))) +
    scale_x_continuous(breaks = seq(min(dat$diex),max(dat$diex))) +
    scale_y_continuous(breaks = seq(min(dat$diey),max(dat$diey)))+
    
    geom_tile(aes(fill=statistic))+
    guides(fill=guide_legend(title='Legend'))+
    
    theme(
        panel.background = element_rect(fill= 'white', color = 'white'),
        panel.grid.major = element_line(color='#E0E0E0'),
        panel.grid.minor = element_line(color='#E0E0E0')
        )+
        
    ggtitle('Wafer Map')+
    facet_wrap(~BinName)+
    scale_fill_gradientn(colors = rainbow(100))