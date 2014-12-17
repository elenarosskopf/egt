
data = read.csv("PDspatialtable.csv",skip=6, header = T)
t(data)
data = as.data.frame(data, header = T)

data$r <- data$cost / ( data$benefit - data$cost )
data$freq_c <- data$count.patches.with..strategy_current....C......count.patches.


agg <-aggregate(data, by=list(data$r), FUN=mean, na.rm=TRUE)
data$expr <- (1 - data$r)


plot(data$r, data$freq_c, las=1,pch=18, xlim=c(0,1),ylim=c(0,1),col="red", 
     main= "Frequency of Cooperators in spatial PD along the cost-benefet ratio",
     xlab="C-B-ratio", ylab="Frequency of Coop.")
lines(data$r, data$expr)
#legend(0.55, 1,c("Frequency", "Expected Frequency"), bty="n", col=c("red", "black"), pch=c(18,20) )
