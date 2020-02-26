setwd("/home/samuel/Documentos/R/Johns_Hopkins/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week04")

# QUESTION 01 -------------------------------------------------------------
        
# load file

fileurl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

download.file(fileurl1, destfile = "idaho.csv")

idaho <- read.csv("idaho.csv", header = TRUE)

# Get Names

idaho_names <- names(idaho)

split01 <- strsplit(idaho_names, split = "wgtp")


# QUESTION 02 -------------------------------------------------------------

fileurl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

download.file(fileurl2, destfile = "gross.csv")

gross <- read.csv("gross.csv", header = FALSE, skip = 5, nrows = 190, 
                  stringsAsFactors = FALSE)

gross$V6 <- as.numeric(gsub(",", "", gross$V5))

mean(gross$V6)

# QUESTION 03 -------------------------------------------------------------

grep("^United", gross$V4)

# # QUESTION 04 -----------------------------------------------------------

fileurl3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(fileurl3, destfile = "edu.csv")

edu <- read.csv("edu.csv", header = TRUE)

df <- merge(gross, edu, by.y = "CountryCode", by.x = "V1")

june <- grep("^Fiscal year end: June", df$Special.Notes)

length(june)


# QUESTION 0S -------------------------------------------------------------

library(quantmod)

library(lubridate)

amzn = getSymbols("AMZN", auto.assign = FALSE)

sampleTimes = index(amzn)

year <- year(sampleTimes)

length(year[year == 2012])

year_2012 <- sampleTimes[year == 2012]

year_2012$wday <- weekdays(year_2012)

length(year_2012[weekdays(year_2012) == "segunda"])
