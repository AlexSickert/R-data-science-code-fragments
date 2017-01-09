
data <-read.csv("household_power_consumption.txt", sep = ";", header = TRUE, as.is = TRUE)

dataSub <- data[grep("^1/2/2007|^2/2/2007", data$Date), ]
dataSub2 <- transform(dataSub, Global_active_power = (as.numeric(Global_active_power)/1000))

dataSub3 <- dataSub2[, c("Date", "Time", "Global_active_power")]
dataSub3["DateTime"] <- NA

dataSub4 <- transform(dataSub3, DateTime = strptime(paste(dataSub3$Date, dataSub3$Time), "%d/%m/%Y %H:%M:%S"))
dataSub5 <- dataSub4[, c("DateTime", "Global_active_power")]

png(filename="plot2.png", width = 480, height = 480, units = "px", bg="white")

plot(dataSub5, type="l", col="black", ylab="Global Active Power (kilowatts)", xlab="")

axis(1, at=1:3, lab=c("Thu", "Fri", "Sat"))
axis(2)

dev.off()



