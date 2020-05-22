
# 0. LOAD PACKAGES --------------------------------------------------------

library(tidyverse)
library(caret)
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
