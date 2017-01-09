
data <-read.csv("household_power_consumption.txt", sep = ";", header = TRUE, as.is = TRUE)

dataSub <- data[grep("^1/2/2007|^2/2/2007", data$Date), ]
dataSub["DateTime"] <- NA
dataSubDateTime <- transform(dataSub, DateTime = strptime(paste(dataSub$Date, dataSub$Time), "%d/%m/%Y %H:%M:%S"))

write.csv(dataSubDateTime, file = "dataSubDateTime.csv")

dataSubNumeric <- dataSubDateTime

write.csv(dataSubNumeric, file = "dataSubNumeric.csv")

dataChart1 <- dataSubNumeric[, c("DateTime", "Sub_metering_1")]
dataChart2 <- dataSubNumeric[, c("DateTime",  "Sub_metering_2")]
dataChart3 <- dataSubNumeric[, c("DateTime", "Sub_metering_3")]

colors <- c("black","red","blue")
titles <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")

png(filename="plot3.png", width = 480, height = 480, units = "px", bg="white")

plot(dataChart1, type="l", col=colors[1], xlab="", ylab="Energy sub metering")
lines(dataChart2, type="l", pch=22, lty=2, col=colors[2])
lines(dataChart3, type="l", pch=23, lty=3, col=colors[3])
legend("topright", lty=1, col = c("black","red","blue"), cex=.75, legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

dev.off()


