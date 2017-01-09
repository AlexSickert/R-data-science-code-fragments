

library(ggplot2) 
library(grid)
library(gridExtra)


# set working directory
setwd('/home/as/Documents/courseradatascience/04-Exploratory-Analysis/Project-2/data')
# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


NEIsubBaltimore <-subset(NEI, NEI$fips == "24510" )
NEIsubCalifornia <-subset(NEI, NEI$fips == "06037" )

SCCVehicle <- SCC[grep(".*(Mobile).*(Road).*", SCC$EI.Sector), ]


#Baltimore
# filter data for Baltimore and make plot
NEIVehicleBaltimore = subset(NEIsubBaltimore, SCC  %in% SCCVehicle$SCC)
NEICombCoalGroupedBaltimore <- aggregate(NEIVehicleBaltimore$Emissions, list(year = NEIVehicleBaltimore$year), sum)
colnames(NEICombCoalGroupedBaltimore) <- c("year","Emissions")

p1 = with(NEICombCoalGroupedBaltimore, plot(year, Emissions, main = "Baltimore", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGroupedBaltimore)
abline(model, lwd = 2)

# California
# filter data for California an make plot
NEIVehicleCalifornia = subset(NEIsubCalifornia, SCC  %in% SCCVehicle$SCC)
NEICombCoalGroupedCalifornia <- aggregate(NEIVehicleCalifornia$Emissions, list(year = NEIVehicleCalifornia$year), sum)
colnames(NEICombCoalGroupedCalifornia) <- c("year","Emissions")

p2 = with(NEICombCoalGroupedCalifornia, plot(year, Emissions, main = "California", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGroupedCalifornia)
abline(model, lwd = 2)

# combine both plots in one plot



p1 = ggplot(data=NEICombCoalGroupedCalifornia, aes(x=year, y=Emissions)) + geom_line(stat="identity") + geom_point()  + ggtitle("California")
p2 = ggplot(data=NEICombCoalGroupedBaltimore, aes(x=year, y=Emissions)) +  geom_line(stat="identity") + geom_point()  + ggtitle("Baltimore")
# p2 = ggplot(data=NEICombCoalGroupedBaltimore, aes(x=year, y=Emissions)) + + geom_line(stat="identity") + geom_point()  + ggtitle("Baltimore")

png(file = "proj-2-q-6.png", bg = "transparent")
print(grid.arrange(p1, p2,  ncol=2))
dev.off()

