# Question 01

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", 
                       destfile = "C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week01/communities.csv")

commu <- read.csv2("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week01/communities.csv",
                   header = TRUE,
                   sep = ",")
length(commu$VAL[commu$VAL >= 1000000])


# Question 03

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", 
              destfile = "C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week01/ngas.csv")

library(data.table)

dat <- fread(file = "C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week01/ngas.csv")
