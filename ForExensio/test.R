#create original data frame
datain1 <- data.frame(field=c('A', 'A', 'B', 'B', 'C', 'C'),
                   trial=c(1, 2, 1, 2, 1, 2),
                   outcome1=c(7, 6, 6, 5, 4, 4),
                   outcome2=c(4, 4, 5, 5, 3, 2))



wafer <- datin1[,field]
#die#
coordnumtab<-aggregate(wafer,  by=list("wafer"=wafer), FUN=length)

#wf aggregation with bin count - not working correctly just a for is needed to run through the biname also
wfagg<-aggregate(wafer, by=list("wafer"=wafer, "biname"=biname), FUN=length)
#wf<- match(wfagg$wafer[1],coordnumtab$wafer)
j=1
while(j < 1+ length(wfagg$wafer)){
wfagg$x[j] <- wfagg$biname[j]*100/(coordnumtab[match(wfagg$wafer[j],coordnumtab$wafer), 2])
j=j+1
}