library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]




library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]



adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)
head(train)
train


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,  grep("IL|diagnosis", value=TRUE , colnames(adData))]
testing = adData[-inTrain,  grep("IL|diagnosis", value=TRUE , colnames(adData))]

log10p1 <- function(x) log10(x+1)

training[grep("IL", value=TRUE , colnames(training))] <- lapply(training[grep("IL", value=TRUE , colnames(training))], log10p1)

preProc <- preProcess(log10(training,method="pca",pcaComp=12)
trainPC <- predict(preProc,training)
modelFit <- train(training$diagnosis ~ .,method="glm",data=trainPC)
testPC <- predict(preProc,testing)
confusionMatrix(testing$diagnosis,predict(modelFit,testPC))
