

library(ggplot2) 


# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# dataSub <- NEI[grep("1999|2002|2005|2008", NEI$year), ]

#NEIsub <-subset(NEI, NEI$year==1999 | NEI$year== 2002|NEI$year==2005|NEI$year==2008)
#head(NEIsub)

NEIsubBaltimore <-subset(NEIsub, NEI$fips == "24510" )
NEIsubTotals <- aggregate(NEIsubBaltimore$Emissions, list(year = NEIsubBaltimore$year, type = NEIsubBaltimore$type), sum)
colnames(NEIsubTotals) <- c("year","type", "Emissions")
head(NEIsubTotals)

qplot(year, Emissions, data = NEIsubTotals, facets = . ~ type, geom = c("point", "smooth"), method = "lm")


SCCCoal <- SCC[grep(".*(Comb).*(Coal).*", SCC$EI.Sector), ]

head(SCCCoal)



NEICombCoal = subset(NEI, SCC  %in% SCCCoal$SCC)
NEICombCoalGrouped <- aggregate(NEICombCoal$Emissions, list(year = NEICombCoal$year), sum)
colnames(NEICombCoalGrouped) <- c("year","Emissions")
head(NEICombCoalGrouped)

with(NEICombCoalGrouped, plot(year, Emissions, main = "have emissions from coal combustion-related sources changed from 1999–2008", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGrouped)
abline(model, lwd = 2)


#now merge both files

