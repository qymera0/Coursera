
# 01 GITHUB API -----------------------------------------------------------

library(httr)
library(jsonlite)
library(httpuv)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/

oauth_endpoints("github")

# 2. To make your own application, register at
#    https://github.com/settings/developers. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.

myapp <- oauth_app("github",
                   key = "6a3db40c718a1aa13492",
                   secret = "f14d6c1dbe261a6dfb9bfaf8e6477ddc8e69be14")


# 3. Get OAuth credentials

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API

gtoken <- config(token = github_token)

req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error

stop_for_status(req)

# Extract content from a request

json1 <- content(req)

# Convert to a data.frame
gitDF = fromJSON(toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 



# 02 SQLDF ----------------------------------------------------------------

library(sqldf)
library(RSQLite)
library(data.table)

acs <- read.csv("~/R/Johns_Hopkins_Coursera/Course_03_Getting_Cleaning_Data/Week02/getdata_data_ss06pid.csv")


url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

f <- file.path(getwd(), "ss06pid.csv")

download.file(url, f)

acs <- data.table(read.csv(f))

# Answer: 

query1 <- sqldf("select pwgtp1 from acs where AGEP < 50")


# 03 SQL ------------------------------------------------------------------

unique(acs$AGEP)

# Answer
# sqldf("select distinct AGEP from acs")


# 04  HTML ----------------------------------------------------------------

connection <- url("http://biostat.jhsph.edu/~jleek/contact.html")

htmlCode <- readLines(connection)

close(connection)

c(nchar(htmlCode[10]), nchar(htmlCode[20]), nchar(htmlCode[30]), nchar(htmlCode[100]))


# 05 FOR dataset type -----------------------------------------------------

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"

lines <- readLines(url, n = 10)

w <- c(1, 9, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3, 5, 4, 1, 3)

colNames <- c("filler", "week", "filler", "sstNino12", "filler", "sstaNino12", 
              "filler", "sstNino3", "filler", "sstaNino3", "filler", "sstNino34", "filler", 
              "sstaNino34", "filler", "sstNino4", "filler", "sstaNino4")

d <- read.fwf(url, w, header = FALSE, skip = 4, col.names = colNames)

d <- d[, grep("^[^filler]", names(d))]

sum(d[, 4])

# Answer: 
# 32426.7
