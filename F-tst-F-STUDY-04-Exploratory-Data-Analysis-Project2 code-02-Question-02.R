
# set working directory
setwd('/home/as/Documents/courseradatascience/04-Exploratory-Analysis/Project-2/data')
# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# ensure only years used we want
NEIsub <-subset(NEI, NEI$year==1999 | NEI$year== 2002|NEI$year==2005|NEI$year==2008)

# filter out data for Baltimore
NEIsubBaltimore <-subset(NEIsub, NEI$fips == "24510" )

# create totals
NEIsubTotals <- aggregate(NEIsubBaltimore$Emissions, list(year = NEIsubBaltimore$year), sum)

#st column names
colnames(NEIsubTotals) <- c("year","Emissions")

#make plot
#boxplot(Emissions ~ year, NEIsubTotals, xlab = "year", ylab = "Emissions")

# make plot with trend line
png(file = "proj-2-q-2.png", bg = "transparent")
with(NEIsubTotals, plot(year, Emissions, main = "PM2.5 decreased in the Baltimore City, Maryland", pch = 20))
model <- lm(Emissions ~ year, NEIsubTotals)
abline(model, lwd = 2)
dev.off()

