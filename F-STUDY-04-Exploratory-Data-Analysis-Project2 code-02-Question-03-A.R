library(ggplot2) 

# set working directory
setwd('/home/as/Documents/courseradatascience/04-Exploratory-Analysis/Project-2/data')
# Load data
NEI <- readRDS("summarySCC_PM25.rds")

# filter Baltimore
NEIsubBaltimore <-subset(NEI, NEI$fips == "24510" )

# create subtotals
NEIsubTotals <- aggregate(NEIsubBaltimore$Emissions, list(year = NEIsubBaltimore$year, type = NEIsubBaltimore$type), sum)

#set column names
colnames(NEIsubTotals) <- c("year","type", "Emissions")
head(NEIsubTotals)

#plot graph into file
png(file = "proj-2-q-3.png", bg = "transparent")
print(qplot(year, Emissions, data = NEIsubTotals, facets = . ~ type, geom = c("point", "smooth"), method = "lm"))
dev.off()

