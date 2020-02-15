# QUESTION 01 -------------------------------------------------------------

link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

getwd()

setwd("C:/Users/Samuel/Desktop/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week03")

download.file(link, destfile = "idaho.csv")

idaho <- read.csv("idaho.csv", header = TRUE)

agricultureLogical <- with(idaho, ACR == 10 & AGS == 6)
        