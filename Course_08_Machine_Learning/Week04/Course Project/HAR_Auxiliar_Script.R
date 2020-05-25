
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

workers <- availableCores()

cl <- makeClusterPSOCK(workers)

registerDoParallel(cl)

# 4.1 Multinomial Regression

mdlLrOr <- train(classe ~., data = harTrnClean, method = 'multinom')

stopCluster(cl)   

summary(mdlLrOr)

registerDoParallel(cl)

mdlLrSc <- train(classe ~., data = harTrnClean, method = 'multinom', preProcess = "scale")

stopCluster(cl)
