

library(ggplot2) 


#  Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 

# dataSub <- NEI[grep("1999|2002|2005|2008", NEI$year), ]

#NEIsub <-subset(NEI, NEI$year==1999 | NEI$year== 2002|NEI$year==2005|NEI$year==2008)
#head(NEIsub)

NEIsubBaltimore <-subset(NEIsub, NEI$fips == "24510" )
NEIsubTotals <- aggregate(NEIsubBaltimore$Emissions, list(year = NEIsubBaltimore$year, type = NEIsubBaltimore$type), sum)
colnames(NEIsubTotals) <- c("year","type", "Emissions")
head(NEIsubTotals)

qplot(year, Emissions, data = NEIsubTotals, facets = . ~ type, geom = c("point", "smooth"), method = "lm")


