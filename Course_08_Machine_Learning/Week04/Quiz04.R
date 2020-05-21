
# QUESTION 01 -------------------------------------------------------------

library(ElemStatLearn)

library(caret)

data(vowel.train)

data(vowel.test)


vowel.train$y <- factor(vowel.train$y)

vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

fitRf <- train(y ~ ., data=vowel.train, method="rf")

fitGbm <- train(y ~ ., data=vowel.train, method="gbm", verbose=FALSE)

prRf <- predict(fitRf, vowel.test)

prGbm <- predict(fitGbm, vowel.test)

print(paste0("RF accuracy = ", confusionMatrix(prRf, vowel.test$y)$overall['Accuracy']))

print(paste0("GBM accuracy = ", confusionMatrix(prGbm, vowel.test$y)$overall['Accuracy']))

agreeIdx <- prRf == prGbm

print(paste0("Agreement accuracy = ", confusionMatrix(vowel.test$y[agreeIdx], prRf[agreeIdx])$overall['Accuracy']))


# QUESTION 02 -------------------------------------------------------------

library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

fitRf <- train(diagnosis ~ ., data=training, method="rf")

fitGbm <- train(diagnosis ~ ., data=training, method="gbm", verbose=FALSE)

fitLda <- train(diagnosis ~ ., data=training, method="lda")

prRf <- predict(fitRf, testing)

prGbm <- predict(fitGbm, testing)

prLda <- predict(fitLda, testing)

combo <- data.frame(prRf, prGbm, prLda, diagnosis = testing$diagnosis)

fitSt <- train(diagnosis ~ ., data=combo, method="rf")

prSt <- predict(fitSt, testing)

print(paste0("RF accuracy = ", confusionMatrix(prRf, testing$diagnosis)$overall['Accuracy']))

print(paste0("GBM accuracy = ", confusionMatrix(prGbm, testing$diagnosis)$overall['Accuracy']))

print(paste0("LDA accuracy = ", confusionMatrix(prLda, testing$diagnosis)$overall['Accuracy']))

print(paste0("Stacked accuracy = ", confusionMatrix(prSt, testing$diagnosis)$overall['Accuracy']))


# QUESTION 03 -------------------------------------------------------------

set.seed(3523)

library(AppliedPredictiveModeling)

library(elasticnet)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

fitLs <- train(CompressiveStrength~., data=training, method = "lasso")

plot.enet(fitLs$finalModel, xvar = "penalty", use.color = TRUE)


# QUESTION 04 -------------------------------------------------------------

library(lubridate)
library(forecast) # For year() function below

fileLink <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"

dat = read.csv(fileLink)

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

fit <- bats(tstrain)

fcast <- forecast(fit, nrow(testing), level = c(95))

plot(fcast)

points(dat$visitsTumblr)

print(sum(fcast$lower <= testing$visitsTumblr & testing$visitsTumblr <= fcast$upper) / dim(testing)[1])


# QUESTION 05 -------------------------------------------------------------

set.seed(325)

library(AppliedPredictiveModeling)

library(e1071)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

svm_model <- svm(CompressiveStrength~., data=training)

#summary(svm_model)

pred <- predict(svm_model, testing)

print(paste0("RMSE = ", sqrt(mean((pred - testing$CompressiveStrength)^2))))

