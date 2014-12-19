
data = read.csv("tasktable.csv",skip=6, header = T)
t(data)
data = as.data.frame(data, header = T)

dataHD = data[which(data$game == '"HD"'),]
datamoore1HD= dataHD[which(dataHD$neighborhood == '"moore_r=1"'),]
datamoore3HD= dataHD[which(dataHD$neighborhood == '"moore_r=3"'),]
dataneumannHD= dataHD[which(dataHD$neighborhood == '"von neumann_r=1"'),]

datamoore1HD$r <- datamoore1HD$cost / (2*datamoore1HD$benefit - datamoore1HD$cost)
datamoore1HD$freq_c <- datamoore1HD$count.patches.with..strategy_current....C../ 2500

datamoore3HD$r <- datamoore3HD$cost / (2*datamoore3HD$benefit - datamoore3HD$cost)
datamoore3HD$freq_c <- datamoore3HD$count.patches.with..strategy_current....C../ 2500

dataneumannHD$r <- dataneumannHD$cost / (2*dataneumannHD$benefit - dataneumannHD$cost)
dataneumannHD$freq_c <- dataneumannHD$count.patches.with..strategy_current....C../ 2500

aggdata1HD <-aggregate(datamoore1HD, by=list(datamoore1HD$r), FUN=mean, na.rm=TRUE)
aggdata2HD <-aggregate(datamoore3HD, by=list(datamoore3HD$r), FUN=mean, na.rm=TRUE)
aggdata3HD <-aggregate(dataneumannHD, by=list(dataneumannHD$r), FUN=mean, na.rm=TRUE)

par(mfrow=c(1,3))

#plot(aggdata1HD$r, aggdata1HD$freq_c, las=1,pch=18,xlab="C-B-ratio", ylab="frequency of coop") #HDm1
#plot(aggdata3HD$r, aggdata3HD$freq_c,las=1, pch=18,  xlab="C-B-ratio", ylab="frequency of coop" ) #HDn1
#plot(aggdata2HD$r, aggdata2HD$freq_c,las=1, pch=18, xlab="C-B-ratio", ylab="frequency of coop" ) #HDm3

ggplot(aggdata1HD, aes(x = r, y=freq_c))+ geom_point() + labs(colour=NULL) + xlim(0,1)+ xlab("C-B-ratio") + ylab("Proportion of Cooperators")
ggplot(aggdata3HD, aes(x = r, y=freq_c))+ geom_point() + labs(colour=NULL) + xlim(0,1)+ xlab("C-B-ratio") + ylab("Proportion of Cooperators")
ggplot(aggdata2HD, aes(x = r, y=freq_c))+ geom_point() + labs(colour=NULL) + xlim(0,1)+ xlab("C-B-ratio") + ylab("Proportion of Cooperators")


dataPD = data[which(data$game == '"PD"'),]
datamoore1= dataPD[which(dataPD$neighborhood == '"moore_r=1"'),]
datamoore3= dataPD[which(dataPD$neighborhood == '"moore_r=3"'),]
dataneumann= dataPD[which(dataPD$neighborhood == '"von neumann_r=1"'),]

datamoore1$r <- datamoore1$cost / (datamoore1$benefit - datamoore1$cost)
datamoore1$freq_c <- datamoore1$count.patches.with..strategy_current....C../ 2500

datamoore3$r <- datamoore3$cost / (datamoore3$benefit - datamoore3$cost)
datamoore3$freq_c <- datamoore3$count.patches.with..strategy_current....C../ 2500

dataneumann$r <- dataneumann$cost / (dataneumann$benefit - dataneumann$cost)
dataneumann$freq_c <- dataneumann$count.patches.with..strategy_current....C../ 2500

aggdata1 <-aggregate(datamoore1, by=list(datamoore1$r), FUN=mean, na.rm=TRUE)
aggdata2 <-aggregate(datamoore3, by=list(datamoore3$r), FUN=mean, na.rm=TRUE)
aggdata3 <-aggregate(dataneumann, by=list(dataneumann$r), FUN=mean, na.rm=TRUE)

#plot(aggdata1$r, aggdata1$freq_c, las=1,pch=18,xlab="C-B-ratio", xlim=c(0,1),ylab="frequency of coop") #PDm1
#plot(aggdata3$r, aggdata3$freq_c,las=1, pch=18, xlab="C-B-ratio",  xlim=c(0,1),ylab="frequency of coop" ) #PDn1
#plot(aggdata2$r, aggdata2$freq_c,las=1, pch=18, xlab="C-B-ratio",  xlim=c(0,1),ylab="frequency of coop" ) #PDm3

ggplot(aggdata1HD, aes(x = r, y=freq_c))+ geom_point() + labs(colour=NULL) + xlim(0,1)+ xlab("C-B-ratio") + ylab("Proportion of Cooperators")
ggplot(aggdata3HD, aes(x = r, y=freq_c))+ geom_point() + labs(colour=NULL) + xlim(0,1)+ xlab("C-B-ratio") + ylab("Proportion of Cooperators")
ggplot(aggdata2HD, aes(x = r, y=freq_c))+ geom_point() + labs(colour=NULL) + xlim(0,1)+ xlab("C-B-ratio") + ylab("Proportion of Cooperators")

par(mfrow=c(1,1))
mtext(" Frequency of Cooperators against c-b-ratio", side = 3, line=-2,outer=T )
