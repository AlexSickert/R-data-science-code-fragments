

stormData <-read.csv("repdata_data_StormData.csv", sep = ",", header = TRUE, as.is = TRUE)

head(stormData)

newdata <- stormData[ which( stormData$FATALITIES > 10), ]

write.csv(newdata, file = "newdata.csv")


