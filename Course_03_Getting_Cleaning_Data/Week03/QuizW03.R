# QUESTION 01 -------------------------------------------------------------

link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

getwd()

setwd("C:/Users/Samuel/Desktop/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week03")

download.file(link, destfile = "idaho.csv")

idaho <- read.csv("idaho.csv", header = TRUE)

agricultureLogical <- with(idaho, ACR == 10 & AGS == 6)
        

# QUESTION 03 -------------------------------------------------------------

link2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

download.file(link2, destfile = "gross.csv")

gross <- read.csv("gross.csv", header = FALSE, skip = 5, nrows = 190)

gross$V6 <- as.numeric(gsub(",", "", gross$V5))

link3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(link3, destfile = "edu.csv")

edu <- read.csv("edu.csv")

df <- merge(gross, edu, by.y = "CountryCode", by.x = "V1")



# QUESTION 04 -------------------------------------------------------------

tapply(df$V2, df$Income.Group, mean, na.rm = TRUE)

# QUESTION 05 -------------------------------------------------------------

library(Hmisc)

df$gdpgroups <- cut2(df$V2, g = 5)

tbl <- table(df$gdpgroups, df$Income.Group)
