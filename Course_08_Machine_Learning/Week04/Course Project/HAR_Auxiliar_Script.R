fileLinkTraining <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" 

fileLinkTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

harTraining <- read.csv(fileLinkTraining)

harTest <- read.csv(fileLinkTest)

str(harTest)
