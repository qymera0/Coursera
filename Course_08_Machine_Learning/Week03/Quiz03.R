library(AppliedPredictiveModeling)

data(segmentationOriginal)

library(caret)

library(rattle)

# QUESTION 01 -------------------------------------------------------------

trainSet <- segmentationOriginal[segmentationOriginal$Case =="Train",]

testSet <- segmentationOriginal[segmentationOriginal$Case =="Test",]

set.seed(125)

model_rpart <- train(Class~., data = trainSet, method = "rpart")

fancyRpartPlot(model_rpart$finalModel)


# QUESTION 03 -------------------------------------------------------------

library(pgmm)

data(olive)

olive = olive[,-1]

olive_rpart <- train(Area ~., data = olive, method = "rpart")

fancyRpartPlot(olive_rpart$finalModel)


# QUESTION 04 -------------------------------------------------------------

library(ElemStatLearn)

data(SAheart)

set.seed(8484)

train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)

trainSA = SAheart[train,]

testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)

regModel <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                  data = trainSA,
                  method = "glm",
                  family = "binomial")

missClassTrain <- missClass(trainSA$chd,predict(regModel,newdata=trainSA))

missClassTest <- missClass(testSA$chd,predict(regModel,newdata=testSA))

missClassTrain


# QUESTION 05 -------------------------------------------------------------

library(ElemStatLearn)

data(vowel.train)

data(vowel.test)

set.seed(33833)

library(randomForest)

vowel.train$y <- as.factor(vowel.train$y)

vowel.test$y <- as.factor(vowel.test$y)

modelRF <- randomForest(y~., data = vowel.train)

order(varImp(modelRF), decreasing = TRUE)
