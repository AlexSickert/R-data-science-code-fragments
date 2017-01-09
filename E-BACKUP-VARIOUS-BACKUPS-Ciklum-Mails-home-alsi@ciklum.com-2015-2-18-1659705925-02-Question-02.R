
#  PM2.5 decreased in the Baltimore City, Maryland
# dataSub <- NEI[grep("1999|2002|2005|2008", NEI$year), ]

#NEIsub <-subset(NEI, NEI$year==1999 | NEI$year== 2002|NEI$year==2005|NEI$year==2008)
#head(NEIsub)

NEIsubBaltimore <-subset(NEIsub, NEI$fips == "24510" )


NEIsubTotals <- aggregate(NEIsubBaltimore$Emissions, list(year = NEIsubBaltimore$year), sum)
#NEIsubTotals <- NEIsubBaltimore
colnames(NEIsubTotals) <- c("year","Emissions")
head(NEIsubTotals)
# NEIsubTotalsDF <- data.frame(NEIsubTotals)
# cols <- c("Year","Emissions")
# colnames(NEIsubTotalsDF) <- "xxx"
# 
# print(NEIsubTotalsDF)
boxplot(Emissions ~ year, NEIsubTotals, xlab = "year", ylab = "Emissions")
#abline(Emissions, lwd=2)
#str(NEIsub)

with(NEIsubTotals, plot(year, Emissions, main = "PM2.5 decreased in the Baltimore City, Maryland", pch = 20))
model <- lm(Emissions ~ year, NEIsubTotals)
abline(model, lwd = 2)

