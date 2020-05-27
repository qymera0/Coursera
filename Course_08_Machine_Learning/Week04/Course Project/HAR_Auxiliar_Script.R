
# 0. LOAD PACKAGES --------------------------------------------------------
library(tidyverse)
library(caret)
library(future)
library(doParallel)
library(heatmaply)
library(factoextra)
library(FactoMineR)
library(nnet)
library(future)
library(doParallel)
library(e1071)

# 1. DATA DOWNLOAD --------------------------------------------------------

fileLinkTraining <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" 

fileLinkTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

harTraining <- read.csv(fileLinkTraining)

harTest <- read.csv(fileLinkTest)

# 2. DATA WRANGLING -------------------------------------------------------

not_all_na <- function(x) any(!is.na(x))

harTstClean <-
        harTest %>%
        select(-c("X", 
                  "user_name", 
                  "raw_timestamp_part_1", 
                  "raw_timestamp_part_2", 
                  "cvtd_timestamp",
                  "problem_id",
                  "num_window")) %>%
        select_if(not_all_na) %>%
        select_if(~n_distinct(.) > 1)
        
harTrnClean <- harTraining[ ,c(names(harTstClean), "classe")]


# 3. EDA ------------------------------------------------------------------

# 3.1 Null performance model

nullPred <- test %>% select("classe")

nullPred$pred.class <- names(sort(table(nullPred$classe), decreasing = TRUE)[1])

print(confusionMatrix(as.factor(nullPred$pred.class), reference = as.factor(nullPred$classe)))

# 3.2 Multicolinearity

corrMat <- harTrnClean %>% select(-classe) %>% mutate_if(is.integer, as.numeric) %>% cor()

heatmaply(corrMat, symm = TRUE, cexRow = .0001, cexCol = .0001, branches_lwd = .1)

pcaCov <- harTrnClean %>% select(-classe) %>% PCA(scale.unit = TRUE, ncp = 10, graph = FALSE)

get_eigenvalue(pcaCov)

fviz_eig(pcaCov)

# 4 MODELING --------------------------------------------------------------

workers <- availableCores() - 1

# 4.1 Multinomial Regression

# Centered and scale

cl <- makeClusterPSOCK(workers)

registerDoParallel(cl)

mdlLrSc <- train(classe ~., data = train, method = 'multinom', preProcess = c("center","scale"))

stopCluster(cl)

registerDoSEQ()

save(mdlLrSc, 
     file = "~/Google Drive/Data Science/Learning/Johns_Hopkins_Coursera/Course_08_Machine_Learning/Week04/Course Project/LogRegScale.RData")

lrScPred <- predict.train(mdlLrSc, newdata = test)

print(confusionMatrix(lrScPred, reference = as.factor(test$classe)))

# Scale with PCA

cl <- makeClusterPSOCK(workers)

registerDoParallel(cl)

mdlLrPCA <- train(classe ~., data = train, method = 'multinom', preProcess = c("center", "scale", "pca"))

stopCluster(cl)

registerDoSEQ()

save(mdlLrPCA, 
     file = "~/Google Drive/Data Science/Learning/Johns_Hopkins_Coursera/Course_08_Machine_Learning/Week04/Course Project/LogRegPca.RData")

lrPCAPred <- predict.train(mdlLrPCA, newdata = test)

print(confusionMatrix(lrPCAPred, reference = as.factor(test$classe)))

# 4.2 Linear Discriminant Analysis

cl <- makeClusterPSOCK(workers)

registerDoParallel(cl)

mdlLDA <- train(classe ~., data = train, method = 'lda', preProcess = c("center", "scale"))

stopCluster(cl)

registerDoSEQ()

LDAPred <- predict.train(mdlLDA, newdata = test)

print(confusionMatrix(mdlLDA, reference = as.factor(test$classe)))

save(mdlLDA, 
     file = "~/Google Drive/Data Science/Learning/Johns_Hopkins_Coursera/Course_08_Machine_Learning/Week04/Course Project/LDA.RData")

# 4.3 Random Forest

cl <- makeClusterPSOCK(workers)

registerDoParallel(cl)

mdlRf <- train(classe ~., data = train, method = 'ranger')

stopCluster(cl)

registerDoSEQ()

rfPred <- predict.train(mdlRf, newdata = test)

print(confusionMatrix(mdlRf, reference = as.factor(test$classe)))

save(mdlRf, 
     file = "~/Google Drive/Data Science/Learning/Johns_Hopkins_Coursera/Course_08_Machine_Learning/Week04/Course Project/mdlRf.RData")

# 4.4 Gradient Boost Machine

cl <- makeClusterPSOCK(workers)

registerDoParallel(cl)

mdlGbm <- train(classe ~., data = train, method = 'gbm')

stopCluster(cl)

registerDoSEQ()

gbmPred <- predict.train(mdlGbm, newdata = test)

print(confusionMatrix(mdlGbm, reference = as.factor(test$classe)))

save(mdlGbm, 
     file = "~/Google Drive/Data Science/Learning/Johns_Hopkins_Coursera/Course_08_Machine_Learning/Week04/Course Project/mdlGbm.RData")
