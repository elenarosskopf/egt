#to plot the results from task 2 we need to do some R coding here: 
#load in the data from the behaviourspace experiment from task 2:
#do not forget to set your working directory properly. 

data = read.csv("data.csv",skip=6, header = T)
t(data)
data = as.data.frame(data, header = T)

#first the snowdrift game results with different neighborhoods

dataHD = data[-c(1:81),]
datamoore1= dataHD[which(dataHD$neighborhood == '"moore_r=1"'),] # 4 neighbors
datamoore3= dataHD[which(dataHD$neighborhood == '"moore_r=3"'),] # 8 neighbors
dataneumann= dataHD[which(dataHD$neighborhood == '"von neumann_r=1"'),] # 3 neighbors 

datamoore1$r <- datamoore1$cost / (2*datamoore1$benefit - datamoore1$cost)
datamoore1$freq_c <- datamoore1$count.patches.with..strategy_current....C../ 2500

datamoore3$r <- datamoore3$cost / (2*datamoore3$benefit - datamoore3$cost)
datamoore3$freq_c <- datamoore3$count.patches.with..strategy_current....C../ 2500

dataneumann$r <- dataneumann$cost / (2*dataneumann$benefit - dataneumann$cost)
dataneumann$freq_c <- dataneumann$count.patches.with..strategy_current....C../ 2500

write.table(dataneumann, file="neumann.csv", sep=",", dec=".")

aggdata1 <-aggregate(datamoore1, by=list(datamoore1$r), FUN=mean, na.rm=TRUE)
datamoore1$expr <- 1 - datamoore1$r
aggdata2 <-aggregate(datamoore3, by=list(datamoore3$r), FUN=mean, na.rm=TRUE)
datamoore3$expr <- 1 - datamoore3$r
aggdata3 <-aggregate(dataneumann, by=list(dataneumann$r), FUN=mean, na.rm=TRUE)
dataneumann$expr <- 1 - dataneumann$r

#plot the thing 

plot(aggdata1$r, aggdata1$freq_c, las=1,pch=18, col="red", main= " Neighborhood Moore with r=1", xlab="C-B-ratio", ylab="frequency of coop")
lines(datamoore1$r, datamoore1$expr)
legend("bottomleft",c("frequency", "expected frequency"),bty ="n", col=c("red", "black"), pch=c(18,20) )

plot(aggdata2$r, aggdata2$freq_c,las=1, pch=18, ylim=c(0,1), col="red",main= " Neighborhood Moore with r=3", xlab="C-B-ratio", ylab="frequency of coop" )
lines(datamoore3$r, datamoore3$expr)
legend("bottomleft",c("frequency", "expected frequency"), bty ="n", col=c("red", "black"), pch=c(18,20) )


plot(aggdata3$r, aggdata3$freq_c,las=1, pch=18, col="red", main= " Neighborhood v.Neumann with r=1", xlab="C-B-ratio", ylab="frequency of coop" )
lines(dataneumann$r, dataneumann$expr)
legend("bottomleft",c("frequency", "expected frequency"), bty ="n", col=c("red", "black"), pch=c(18,20) )


#still missing pair approximations

##############DO EVERYTHING WITH PD##################

data = read.csv("data.csv",skip=6, header = T)
t(data)
data = as.data.frame(data, header = T)

dataPD = data[-c(82:162),]
datamoorePD1= dataPD[which(dataPD$neighborhood == '"moore_r=1"'),]
datamoorePD3= dataPD[which(dataPD$neighborhood == '"moore_r=3"'),]
dataneumannPD= dataPD[which(dataPD$neighborhood == '"von neumann_r=1"'),]

datamoorePD1$r <- datamoorePD1$cost / (2*datamoorePD1$benefit - datamoorePD1$cost)
datamoorePD1$freq_c <- datamoorePD1$count.patches.with..strategy_current....C../ 2500

datamoorePD3$r <- datamoorePD3$cost / (2*datamoorePD3$benefit - datamoorePD3$cost)
datamoorePD3$freq_c <- datamoorePD3$count.patches.with..strategy_current....C../ 2500

dataneumannPD$r <- dataneumannPD$cost / (2*dataneumannPD$benefit - dataneumannPD$cost)
dataneumannPD$freq_c <- dataneumannPD$count.patches.with..strategy_current....C../ 2500

write.table(dataneumann, file="neumann.csv", sep=",", dec=".")

aggdata1 <-aggregate(datamoorePD1, by=list(datamoorePD1$r), FUN=mean, na.rm=TRUE)
datamoorePD1$expr <- (1 - datamoorePD1$r)
aggdata2 <-aggregate(datamoorePD3, by=list(datamoorePD3$r), FUN=mean, na.rm=TRUE)
datamoorePD3$expr <- 1 - datamoorePD3$r
aggdata3 <-aggregate(dataneumannPD, by=list(dataneumannPD$r), FUN=mean, na.rm=TRUE)
dataneumannPD$expr <- 1 - dataneumannPD$r


plot(aggdata1$r, aggdata1$freq_c, las=1,pch=18, ylim=c(0,1),col="red", main= " Neighborhood Moore with r=1", xlab="C-B-ratio", ylab="frequency of coop")
lines(datamoore1$r, datamoore1$expr)
legend(0.55, 1,c("frequency", "expected frequency"), bty="n", col=c("red", "black"), pch=c(18,20) )

plot(aggdata2$r, aggdata2$freq_c,las=1, pch=18, ylim=c(0,1), col="red",main= " Neighborhood Moore with r=3", xlab="C-B-ratio", ylab="frequency of coop" )
lines(datamoore3$r, datamoore3$expr)
legend(0.55, 1,c("frequency", "expected frequency"),bty="n",  col=c("red", "black"), pch=c(18,20) )


plot(aggdata3$r, aggdata3$freq_c,las=1, pch=18, ylim=c(0,1), col="red", main= " Neighborhood v.Neumann with r=1", xlab="C-B-ratio", ylab="frequency of coop" )
lines(dataneumann$r, dataneumann$expr)
legend(0.55, 1,c("frequency", "expected frequency"), bty="n", col=c("red", "black"), pch=c(18,20) )


#still missing pair approximations