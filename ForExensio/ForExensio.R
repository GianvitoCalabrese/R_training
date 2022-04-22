#GENERAL USED NAMES & TYPE DEFINITION
#datin[#]: TABLE (INPUT)
#datout[#]: TABLE (OUTPUT)
#colin[#]: COL (INPUT)
#colout[#]: COL (OUTPUT)
#varin[#]: PROPERTY (INPUT)
#varout[#]: PROPERTY (OUTPUT)

#Script developed in order to obtain the colnames of a table and the realize 
#Input Parameters: datin1
#Output Parameters: datout2

columns<-colnames(datin1)
datout2<- unique(datin1[grep("Bin",columns)]) %>% drop_na()


#Script developed in order to count the diff unique entries in a certain column 
#Input Parameters: datin1
#Output Parameters: varout1
 
varout1 <- length(unique(datin1[,varin1]))

#Script developed in order  
#Input Parameters: dat
#Output Parameters: dat

for(i in 1:nrow(datin1)){ 
	if grepl('^[A-Za-z]+$', str_replace(dat[i,"Data"], c(".", "-"), ""), perl = T){
			x <- dat[i,"Parameter"]
		}
	 dat[i,"PPU"] <- x 
	}


#Script developed in order  
#Input Parameters: datin1, var1
#Output Parameters: datout1


for (i in colnames(datin1)){
	if(grepl( var1, i, fixed = TRUE)){
	datout1 <- datin1[,i]
	}
}

#Script developed in order  
#Input Parameters: dat
#Output Parameters: dat

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


#Script developed in order  
#Input Parameters: datin1
#Output Parameters: dat

datout <- datin1 %>% select_if(grepl('^.._'))


#Script developed in order to save all the table in a report in the 'mytable.RData' 
#Input Parameters: value <- 'B:/Public/zbmbcf - Gianvito Calabrese/'

path <- var1
save.image(paste( path,"mytable.RData"))


#Script developed in order . Taken a certain field  
#Input Parameters: dat
#Output Parameters: dat

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

names(wfagg)[names(wfagg) == "x"] <- "count (%)"


second <-aggregate(date, by=list("wafer"= wafer), max) 
names(second)[names(second) == "x"] <- "time"
class(second$time)<-c('POSIXt','POSIXct')
total<-merge(wfagg, second, by="wafer", all.x=TRUE)


biname <- t(unique(wfagg$biname))
colnames(biname) <- biname[1,]

#Script developed in order to rebuild 
#Input Parameters: datin1
#Output Parameters: dat


test_array <- c()
datin1$BIN_INDEX <- paste0("B", datin1$BIN_INDEX)
datin1_match <- datin1[, c("BIN_INDEX", "BIN_NAME")]
i=1
for (x in colnames(bin_table)){
#print(colnames(bin_table)[i])
if( is.na (match(colnames(bin_table)[i], datin1_match$BIN_INDEX) ) ){

test_array = append( test_array ,colnames(bin_table)[i] )
} else {
   test_array = append( test_array ,datin1_match$BIN_NAME[match(colnames(bin_table)[i], datin1_match$BIN_INDEX)] )
   }
i=i+1
}
colnames(bin_table) <- test_array




#stack the third and fourth columns
stacked<-cbind(data[1:2], stack(data[3:4]))
View(stacked)


#

StackDF <- function(df, indxCols, stackCols, stackParName="Parameter", stackValName="Value") {
   dfStack <- NULL
   if(length(stackCols) == 0) {
      return(df)
   }
   for(i in 1:length(stackCols)) {
      dfTmp <- df[,c(indxCols,stackCols[i])]
      names(dfTmp)[ncol(dfTmp)] <- stackValName
      dfTmp[,stackParName] <- rep(stackCols[i],nrow(dfTmp))
      if(is.null(dfStack)) {
         dfStack <- dfTmp
      } else {
         dfStack <- rbind(dfStack, dfTmp)
      }
   }
   return(dfStack)

   #


