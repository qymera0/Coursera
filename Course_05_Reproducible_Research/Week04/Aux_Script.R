# 00 LOAD PACKAGES --------------------------------------------------------

library(tidyverse)
library(lubridate)

# 01 DOWNLOAD AND LOAD DATA -----------------------------------------------

# Setting working directory

setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_05_Reproducible_Research/Week04")

# Download file

fileLink <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(fileLink, 
              dest="Dataset/tmp.bz2", 
              method="curl")

# Load data

weather <- read_csv("Dataset/tmp.bz2", 
                    col_types = cols(BGN_DATE = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                     BGN_TIME = col_time(format = "%H%M"), 
                                     CROPDMGEXP = col_character()))

# 02 DATA PREPARATION -----------------------------------------------------

# Select only columns of interest for whole analysis

weatherClean <- weather %>%
        select(BGN_DATE, BGN_TIME, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

rm(weather) # remove to save RAM

# Select rows for Question 01

fatalities <- weatherClean %>%
        select(BGN_DATE, BGN_TIME, EVTYPE, FATALITIES) %>%
        filter(FATALITIES > 0)

injuries <- weatherClean %>%
        select(BGN_DATE, BGN_TIME, EVTYPE, INJURIES) %>%
        filter(INJURIES > 0)


# Select rows for QUESTION 2

economic <- weatherClean %>%
        select(BGN_DATE, BGN_TIME, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

rm(weatherClean) # remove to save RAM

table(economic$PROPDMGEXP)

table(economic$CROPDMGEXP)

economic <- economic %>%
        filter(grepl("m|k|b", PROPDMGEXP, ignore.case = TRUE)) %>%
        filter(grepl("m|k|b", CROPDMGEXP, ignore.case = TRUE)) %>%
        str_replace_all(economic$PROPDMGEXP, c("k" = "1e+03", "m" = "1e+06", b = "1e+09"))
        

# Summary of data

summary(fatalities)

summary(injuries)

# Determine with fatalities are top 5

fatTop <-fatalities %>%
         group_by(EVTYPE) %>%
         summarise(fat.total = sum(FATALITIES)) %>%
         top_n(n = 5, wt = fat.total)

        
# Summarise by eventype and year

fatTime <- fatalities %>%
        group_by(EVTYPE, year(BGN_DATE)) %>%
        summarise(fat.total = sum(FATALITIES)) %>%
        filter(EVTYPE %in% fatTop$EVTYPE) %>%
        rename(fat.year = `year(BGN_DATE)`)

fYearSeries <- ggplot(fatTime, aes(x = fat.year, y = fat.total))

fYearSeries + geom_line(aes(color = EVTYPE))
