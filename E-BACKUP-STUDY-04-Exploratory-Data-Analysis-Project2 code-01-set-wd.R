
setwd('/home/as/Documents/courseradatascience/04-Exploratory-Analysis/Project-2/data')

print("start")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEIsub <-subset(NEI, NEI$year==1999 | NEI$year== 2002|NEI$year==2005|NEI$year==2008)

print("done")
#head(NEI)
# str(SCC)