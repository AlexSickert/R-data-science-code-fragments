

# Quiz 3

# Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
training = segmentationOriginal[segmentationOriginal$Case == "Train",]
testing = segmentationOriginal[segmentationOriginal$Case == "Test",]
set.seed(125)
M <- train(Class ~ ., data=training, method="rpart")
M
M$finalModel
plot(M$finalModel)
text(M$finalModel)

# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]
M <- train(Area ~ ., data=olive, method="rpart")
newdata = as.data.frame(t(colMeans(olive)))
newdata
predict(M, newdata)
M$finalModel

# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
M <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method="glm", family="binomial")
M
M$finalModel
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd, predict(M, testSA))
missClass(trainSA$chd, predict(M, trainSA))

# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
M <- train(y ~ ., data=vowel.train, method="rf")
varImp(M)
