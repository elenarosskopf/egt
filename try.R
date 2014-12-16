

data = read.csv("data.csv",skip=6, header = T)
t(data)
data = as.data.frame(data, header = T)

dataHD = data[-c(1:81),]
datamoore1= dataHD[which(dataHD$neighborhood == '"moore_r=1"'),]
datamoore3= dataHD[which(dataHD$neighborhood == '"moore_r=3"'),]
dataneumann= dataHD[which(dataHD$neighborhood == '"von neumann_r=1"'),]

datamoore1$r <- datamoore1$cost / (2*datamoore1$benefit - datamoore1$cost)
datamoore1$freq_c <- datamoore1$count.patches.with..strategy_current....C../ 2500

datamoore3$r <- datamoore3$cost / (2*datamoore3$benefit - datamoore3$cost)
datamoore3$freq_c <- datamoore3$count.patches.with..strategy_current....C../ 2500

dataneumann$r <- dataneumann$cost / (2*dataneumann$benefit - dataneumann$cost)
dataneumann$freq_c <- dataneumann$count.patches.with..strategy_current....C../ 2500

write.table(dataneumann, file="neumann.csv", sep=",", dec=".")

aggdata1 <-aggregate(datamoore1, by=list(datamoore1$r), FUN=mean, na.rm=TRUE)
aggdata2 <-aggregate(datamoore3, by=list(datamoore3$r), FUN=mean, na.rm=TRUE)
aggdata3 <-aggregate(dataneumann, by=list(dataneumann$r), FUN=mean, na.rm=TRUE)


plot(aggdata1$r, aggdata1$freq_c, las=1,pch=18, main= " Neighborhood Moore with r=1", xlab="C-B-ratio", ylab="frequency of coop")

plot(aggdata2$r, aggdata2$freq_c,las=1, pch=18, main= " Neighborhood Moore with r=3", xlab="C-B-ratio", ylab="frequency of coop" )

plot(aggdata3$r, aggdata3$freq_c,las=1, pch=18, main= " Neighborhood v.Neumann with r=1", xlab="C-B-ratio", ylab="frequency of coop" )



