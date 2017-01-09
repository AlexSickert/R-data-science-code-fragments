



# set working directory
setwd('/home/as/Documents/courseradatascience/04-Exploratory-Analysis/Project-2/data')
# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#filter Baltimore
NEIsubBaltimore <-subset(NEI, NEI$fips == "24510" )

# filter by veihcles
SCCVehicle <- SCC[grep(".*(Mobile).*(Road).*", SCC$EI.Sector), ]


# filter data and then aggregate
NEIVehicleBaltimore = subset(NEIsubBaltimore, SCC  %in% SCCVehicle$SCC)
NEICombCoalGrouped <- aggregate(NEIVehicleBaltimore$Emissions, list(year = NEIVehicleBaltimore$year), sum)
colnames(NEICombCoalGrouped) <- c("year","Emissions")
head(NEICombCoalGrouped)

# make plot
png(file = "proj-2-q-5.png", bg = "transparent")
with(NEICombCoalGrouped, plot(year, Emissions, main = "Baltimore City: emissions from vehicle sources", pch = 20))
model <- lm(Emissions ~ year, NEICombCoalGrouped)
abline(model, lwd = 2)
dev.off()



