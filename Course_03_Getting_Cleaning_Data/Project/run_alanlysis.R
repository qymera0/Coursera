
# 0 OPEN FILES ------------------------------------------------------------

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Project")

# Train datasets

xTrain <- read.table("UCI HAR Dataset/train/X_train.txt", 
                     quote = "\"", 
                     comment.char = "", 
                     stringsAsFactors = FALSE) 

yTrain <- read.table("UCI HAR Dataset/train/Y_train.txt", 
                     quote = "\"", 
                     comment.char = "", 
                     stringsAsFactors = FALSE)

names(yTrain) <- "activity"

subjTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                        quote = "\"", 
                        comment.char = "", 
                        stringsAsFactors = FALSE)

names(subjTrain) <- "subject"

# Test Sets

xTest <- read.table("UCI HAR Dataset/test/X_test.txt", 
                     quote = "\"", 
                     comment.char = "", 
                     stringsAsFactors = FALSE) 

yTest <- read.table("UCI HAR Dataset/test/Y_test.txt", 
                    quote = "\"", 
                    comment.char = "", 
                    stringsAsFactors = FALSE)

names(yTest) <- "activity"

subjTest <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                        quote = "\"", 
                        comment.char = "", 
                        stringsAsFactors = FALSE)

names(subjTest) <- "subject"

# Activity Labels

acLabels <- read.table("UCI HAR Dataset/activity_labels.txt", 
                       quote = "\"", 
                       comment.char = "", 
                       stringsAsFactors = FALSE) 

featNames <- read.table("UCI HAR Dataset/features.txt", 
                        quote = "\"", 
                        comment.char = "", 
                        stringsAsFactors = FALSE)


# 1 MERGING DATASETS ------------------------------------------------------

# subject, x and y

test <- cbind(xTest, subjTest, yTest)

train <- cbind(xTrain, subjTrain,  yTrain)

# Train and test

dfComplete <- rbind(train, test)

# 2 MEAN AND SD -----------------------------------------------------------

# Variables with mean

meanVar <- grep("mean\\(\\)", featNames$V2) # First column of df is subject

# Variables with SD

sdVar <- grep("std\\(\\)", featNames$V2) # First column of df is subject

# Select only mean and SD measurements and add anser column

dfSelected <- dfComplete[ ,c(meanVar, sdVar, 562:563)]

# 3 RENAME ACTIVITIES -----------------------------------------------------

dfSelected$activity <- plyr::mapvalues(dfSelected$activity , 
                                   from = sort(unique(dfSelected$activity)),
                                   to = acLabels$V2)


# 4 RENAME VARIABLES ------------------------------------------------------

colnames(dfSelected)[colnames(dfSelected) %in% c(names(dfSelected[ ,-c(67:68)]))] <- featNames$V2[c(meanVar, sdVar)]

# 5 AVERAGE FOR ACTIVITY AND SUBJECT --------------------------------------

library(dplyr)

average <- dfSelected %>%
        group_by(subject, activity) %>%
        summarise_all(mean)
        

# # 6 SAVING DATASET ------------------------------------------------------

write.table(average, file = "average.txt", row.name = FALSE)
