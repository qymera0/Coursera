# Question 01

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", 
                       destfile = "C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week01/communities.csv")

commu <- read.csv2("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week01/communities.csv",
                   header = TRUE,
                   sep = ",")

length(commu$VAL[!is.na(commu$VAL) & commu$VAL==24])


# Question 03

fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

download.file(url=fileUrl1, destfile="gov_NGAP.xlsx", mode="w", method="curl")

dateDownloaded <- date()

print(dateDownloaded)

library(xlsx)

rowIndex <- 18:23

colIndx <- 7:15

dat <- read.xlsx(file="gov_NGAP.xlsx",sheetIndex=1,colIndex=colIndx,startRow=18, endRow=23, header=TRUE)


# Question 4

library(XML)

fileUrl2 <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

doc <- xmlTreeParse(file=fileUrl2, useInternal=TRUE)

rootNode <- xmlRoot(doc)

xmlName(rootNode)

names(rootNode)

rootNode[[1]][[1]]

zipcode <- xpathSApply(rootNode,"//zipcode",xmlValue)

length(zipcode[zipcode==21231])

# Question 5

fileUrl3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week01/")

download.file(url=fileUrl3, destfile="fsspid.csv", mode="w", method="curl")

dateDownloaded <- date()

print(dateDownloaded)

library(data.table)

DT <- fread(input="fsspid.csv", sep=",")

library(microbenchmark)

mbm = microbenchmark(

a1 = DT[,mean(pwgtp15),by=SEX],

a2 = tapply(DT$pwgtp15,DT$SEX,mean),

a3 = mean(DT$pwgtp15,by=DT$SEX),

a4 = sapply(split(DT$pwgtp15,DT$SEX),mean),

a5 = mean(DT[DT$SEX==1,]$pwgtp15), 

a6 = mean(DT[DT$SEX==2,]$pwgtp15)

)
