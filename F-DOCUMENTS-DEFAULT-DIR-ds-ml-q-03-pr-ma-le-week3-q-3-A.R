# Question 1
library(AppliedPredictiveModeling)
library(caret)

data(segmentationOriginal)

set.seed(125)

#inTrain <- createDataPartition(segmentationOriginal$Case, list=FALSE)
inTrain <- data$Case == "Train"
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

fit <- train(Class ~ ., data=training, method='rpart')

# Show decision tree: PS, WS, PS, Not possible to predict.
plot(fit$finalModel, uniform=T)
text(fit$finalModel, cex=0.8)

# Question 3
library(pgmm)

data(olive)
olive = olive[,-1]

inTrain <- createDataPartition(olive$Area, list=FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]

fit <- train(Area ~ ., data=training, method='rpart')

newdata = as.data.frame(t(colMeans(olive)))

# Result is strange because it should be a qualitative value in the range of 1-8, not a float value??
predict(fit, newdata=newdata)

# Question 4
library(ElemStatLearn)

data(SAheart)

set.seed(8484)

training = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[training,]
testSA = SAheart[-training,]

set.seed(13234)

fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method='glm', family='binomial')

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(fit, trainSA))
missClass(testSA$chd, predict(fit, testSA))

# Question 5
library(ElemStatLearn)

data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

fit <- randomForest(y ~., data=vowel.train)

# 2 1 5 6 8 4 3 9 7 10.
order(varImp(fit), decreasing=TRUE)




     