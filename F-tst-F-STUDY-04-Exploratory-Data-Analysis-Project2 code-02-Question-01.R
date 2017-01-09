# set working directory
setwd('/home/as/Documents/courseradatascience/04-Exploratory-Analysis/Project-2/data')
# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# ensure only years used we want
NEIsub <-subset(NEI, NEI$year==1999 | NEI$year== 2002|NEI$year==2005|NEI$year==2008)

#create subtotals
NEIsubTotals <- aggregate(NEIsub$Emissions, list(year = NEIsub$year), sum)
#convert to data frame
NEIsubTotalsDF <- data.frame(NEIsubTotals)
#set column names
cols <- c("Year","Emissions")
colnames(NEIsubTotalsDF) <- cols
#plot chart
png(file = "proj-2-q-1.png", bg = "transparent")
boxplot(Emissions ~ Year, NEIsubTotalsDF, xlab = "Year", ylab = "Emissions")
dev.off()
