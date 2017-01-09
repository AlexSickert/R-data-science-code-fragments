
data <-read.csv("household_power_consumption.txt", sep = ";", header = TRUE, as.is = TRUE)


png(filename="plot4.png", width = 480, height = 480, units = "px", bg="white")

# create layout with 4 areas/boxes

par( mfrow = c( 2, 2 ) )

# plot for first box =============================================

dataSub <- data[grep("^1/2/2007|^2/2/2007", data$Date), ]
dataSub2 <- transform(dataSub, Global_active_power = (as.numeric(Global_active_power)/1000))

dataSub3 <- dataSub2[, c("Date", "Time", "Global_active_power")]
dataSub3["DateTime"] <- NA

dataSub4 <- transform(dataSub3, DateTime = strptime(paste(dataSub3$Date, dataSub3$Time), "%d/%m/%Y %H:%M:%S"))
dataSub5 <- dataSub4[, c("DateTime", "Global_active_power")]

plot(dataSub5, type="l", col="black", ylab="Global Active Power (kilowatts)", xlab="")

# plot for second box =============================================

dataSub <- data[grep("^1/2/2007|^2/2/2007", data$Date), ]
dataSub["DateTime"] <- NA
dataSub <- transform(dataSub, DateTime = strptime(paste(dataSub3$Date, dataSub3$Time), "%d/%m/%Y %H:%M:%S"))
dataSubVoltage <- dataSub[, c("DateTime", "Voltage")]
plot(dataSubVoltage, type="l", col="black", ylab="Voltage", xlab="datetime")

# plot for third box =============================================

dataChart1 <- dataSub[, c("DateTime", "Sub_metering_1")]
dataChart2 <- dataSub[, c("DateTime",  "Sub_metering_2")]
dataChart3 <- dataSub[, c("DateTime", "Sub_metering_3")]

colors <- c("black","red","blue")
titles <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")

plot(dataChart1, type="l", col=colors[1], xlab="", ylab="Energy sub metering")
lines(dataChart2, type="l", pch=22, lty=2, col=colors[2])
lines(dataChart3, type="l", pch=23, lty=3, col=colors[3])
legend("topright", lty=1, col = c("black","red","blue"), cex=.75, legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

# plot for fourth box =============================================

dataSub <- data[grep("^1/2/2007|^2/2/2007", data$Date), ]
dataSub["DateTime"] <- NA
dataSub <- transform(dataSub, DateTime = strptime(paste(dataSub3$Date, dataSub3$Time), "%d/%m/%Y %H:%M:%S"))
dataSubVoltage <- dataSub[, c("DateTime", "Global_reactive_power")]
plot(dataSubVoltage, type="l", col="black", ylab="Global_reactive_power", xlab="datetime")

dev.off()
