

library(ggplot2) 
library(grid)
library(gridExtra)


# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 

#   Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
#  in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor 
#   vehicle emissions?

NEIsubBaltimore <-subset(NEI, NEI$fips == "24510" )
NEIsubCalifornia <-subset(NEI, NEI$fips == "06037" )

SCCVehicle <- SCC[grep(".*(Mobile).*(Road).*", SCC$EI.Sector), ]


#Baltimore

NEIVehicleBaltimore = subset(NEIsubBaltimore, SCC  %in% SCCVehicle$SCC)
NEICombCoalGroupedBaltimore <- aggregate(NEIVehicleBaltimore$Emissions, list(year = NEIVehicleBaltimore$year), sum)
colnames(NEICombCoalGroupedBaltimore) <- c("year","Emissions")

p1 = with(NEICombCoalGroupedBaltimore, plot(year, Emissions, main = "Baltimore", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGroupedBaltimore)
abline(model, lwd = 2)

# California

NEIVehicleCalifornia = subset(NEIsubCalifornia, SCC  %in% SCCVehicle$SCC)
NEICombCoalGroupedCalifornia <- aggregate(NEIVehicleCalifornia$Emissions, list(year = NEIVehicleCalifornia$year), sum)
colnames(NEICombCoalGroupedCalifornia) <- c("year","Emissions")

p2 = with(NEICombCoalGroupedCalifornia, plot(year, Emissions, main = "California", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGroupedCalifornia)
abline(model, lwd = 2)

#grid.arrange(p1, p2,  ncol=2)
#ggplot(NEICombCoalGroupedCalifornia)
#NEICombCoalGroupedCalifornia, plot(year, Emissions, main = "California", pch = 20)

p1 = ggplot(data=NEICombCoalGroupedCalifornia, aes(x=year, y=Emissions)) + geom_line(stat="identity") + geom_point()  + ggtitle("California")
p2 = ggplot(data=NEICombCoalGroupedBaltimore, aes(x=year, y=Emissions)) +  geom_line(stat="identity") + geom_point()  + ggtitle("Baltimore")
# p2 = ggplot(data=NEICombCoalGroupedBaltimore, aes(x=year, y=Emissions)) + + geom_line(stat="identity") + geom_point()  + ggtitle("Baltimore")

grid.arrange(p1, p2,  ncol=2)

