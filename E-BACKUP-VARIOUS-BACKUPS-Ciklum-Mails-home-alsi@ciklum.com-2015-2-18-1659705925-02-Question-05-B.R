

library(ggplot2) 


# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 

NEIsubBaltimore <-subset(NEI, NEI$fips == "24510" )

SCCVehicle <- SCC[grep(".*(Mobile).*(Road).*", SCC$EI.Sector), ]

#head(SCCVehicle)

NEIVehicleBaltimore = subset(NEIsubBaltimore, SCC  %in% SCCVehicle$SCC)
NEICombCoalGrouped <- aggregate(NEIVehicleBaltimore$Emissions, list(year = NEIVehicleBaltimore$year), sum)
colnames(NEICombCoalGrouped) <- c("year","Emissions")
head(NEICombCoalGrouped)

with(NEICombCoalGrouped, plot(year, Emissions, main = "have emissions from coal combustion-related sources changed from 1999–2008", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGrouped)
abline(model, lwd = 2)


#now merge both files

