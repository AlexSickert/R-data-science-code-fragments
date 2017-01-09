

data <-read.csv("household_power_consumption.txt", sep = ";", header = TRUE, as.is = TRUE)

dataSub <- data[grep("^1/2/2007|^2/2/2007", data$Date), ]

dataSub2 <- transform(dataSub, Global_active_power = (as.numeric(Global_active_power)))

png(filename="plot1.png", width = 480, height = 480, units = "px", bg="white")

hist(dataSub2$Global_active_power, col="red", main = "Global Active Power", xlab="Global Active Power (kilowatts)")

dev.off()

