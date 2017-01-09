
#
# dataSub <- NEI[grep("1999|2002|2005|2008", NEI$year), ]

#NEIsub <-subset(NEI, NEI$year==1999 | NEI$year== 2002|NEI$year==2005|NEI$year==2008)
# head(NEIsub)

#NEIsub2002 <-subset(NEI, NEI$year== 2002)
#head(NEIsub2002)

#NEIclean <-subset(NEIsub, NEI$Emissions < 10) 


 #NEIsubTwoCol <- NEIsub[, c(NEIsub$Emissions, NEIsub$year)]

# library(datasets)
# airquality <- transform(airquality, Month = factor(Month))
 #boxplot(Emissions ~ year, NEIclean, xlab = "Year", ylab = "Emissions")

#NEIsubTotals <- NEIsub[ , c(NEIsub$Emissions,NEIsub$year)]

#head(NEIsubTotals)

#NEIsubTotals <- tapply(NEIsub$Emissions, NEIsub$year, sum)
NEIsubTotals <- aggregate(NEIsub, NEIsub$year, sum)
head(NEIsubTotals)
NEIsubTotalsDF <- data.frame(NEIsubTotals)
cols <- c("Year","Emissions")
colnames(NEIsubTotalsDF) <- "xxx"

print(NEIsubTotalsDF)
boxplot(Emissions ~ Year, NEIsubTotalsDF, xlab = "Year", ylab = "Emissions")
