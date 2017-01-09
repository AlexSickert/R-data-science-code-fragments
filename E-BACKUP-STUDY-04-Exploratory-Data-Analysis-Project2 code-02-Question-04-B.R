

library(ggplot2) 


# set working directory
setwd('/home/as/Documents/courseradatascience/04-Exploratory-Analysis/Project-2/data')
# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# filter coal combustion parameters
SCCCoal <- SCC[grep(".*(Comb).*(Coal).*", SCC$EI.Sector), ]

# filter data by the previously selected parameters
NEICombCoal = subset(NEI, SCC  %in% SCCCoal$SCC)

# aggregate data
NEICombCoalGrouped <- aggregate(NEICombCoal$Emissions, list(year = NEICombCoal$year), sum)

# new colun names
colnames(NEICombCoalGrouped) <- c("year","Emissions")

# make plot
png(file = "proj-2-q-4.png", bg = "transparent")
with(NEICombCoalGrouped, plot(year, Emissions, main = "Emissions from coal combustion-related sources decreased from 1999–2008", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGrouped)
abline(model, lwd = 2)
dev.off()

