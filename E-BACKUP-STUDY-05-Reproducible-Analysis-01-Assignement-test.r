
###library(plyr)
library(timeDate)

myData <-read.csv("activity.csv", sep = ",", header = TRUE, as.is = TRUE)

head(myData)

res <- aggregate(myData["steps"], by=myData[c("interval")], FUN=mean, na.rm=TRUE)
print(res)


rowIndex = which.max( res[,2] )
print(res[rowIndex,])



sum(is.na(myData$steps))



myData$day <- isWeekend(as.Date(myData$date))

head(myFilledData)

myFilledData$weekend <- factor(isWeekend(as.Date(myFilledData$date)))
head(myFilledData)
## print(myFilledData)

plot(myFilledData$interval, myFilledData$steps)
##plot.ts(myFilledData$interval, myFilledData$steps, plot.type = "multiple")


library(lattice)
library(datasets)

myFilledData$weekend <- factor(myFilledData$weekend, labels = c("Weekday", "Weekend"))

res <- aggregate(myFilledData["steps"], by=myFilledData[c("interval", "weekend")], FUN=mean, na.rm=TRUE)
print(res)


xyplot(steps ~ interval | weekend , data = res, type = "l", layout = c(1,2))