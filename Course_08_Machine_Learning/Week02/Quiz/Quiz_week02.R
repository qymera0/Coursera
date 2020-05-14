
# QUESTION 01 -------------------------------------------------------------

library(AppliedPredictiveModeling)
library(caret)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)

training = adData[trainIndex,]

testing = adData[-trainIndex,]


# QUESTION 02 -------------------------------------------------------------

library(AppliedPredictiveModeling)

data(concrete)

library(caret)

set.seed(1000)

inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]

training = mixtures[ inTrain,]

testing = mixtures[-inTrain,]

index <- seq_along(1:nrow(training))

ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
        theme_bw()

cutCS <- cut2(training$CompressiveStrength, g = 4)

summary(cutCS)

ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
        theme_bw()

featurePlot(x = training[, names], y = cutCS, plot = "box")

# QUESTION 03 -------------------------------------------------------------

library(AppliedPredictiveModeling)

data(concrete)

library(caret)

set.seed(1000)

inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]

training = mixtures[ inTrain,]

testing = mixtures[-inTrain,]

hist(training$Superplasticizer)

hist(log(training$Superplasticizer+1))


# QUESTION 04 -------------------------------------------------------------

library(caret)

library(AppliedPredictiveModeling)

set.seed(3433)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

IL_str <- grep("^IL", colnames(training), value = TRUE)

preProc <- prcomp(training[IL_str])

summary(preProc)


# QUESTION 05 -------------------------------------------------------------

library(caret)

library(AppliedPredictiveModeling)

library(e1071)

set.seed(3433)

data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[inTrain, ]

testing = adData[-inTrain, ]

## grep the predictors starting with 'IL'

IL_str <- grep("^IL", colnames(training), value = TRUE)

## make a subset of these predictors

predictors_IL <- predictors[ ,IL_str]

df <- data.frame(diagnosis, predictors_IL)

## train the data using the first method

modelFit <- train(diagnosis ~ ., method = "glm", data = training)

predictions <- predict(modelFit, newdata = testing)

## get the confustion matrix for the first method

C1 <- confusionMatrix(predictions, testing$diagnosis)

print(C1)

A1 <- C1$overall[1]

## do similar steps with the caret package

modelFit <- train(diagnosis ~ ., 
                  method = "glm", 
                  preProcess = "pca", 
                  data = training, 
                  trControl = trainControl(preProcOptions = list(thresh = 0.8)))

C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))

print(C2)
