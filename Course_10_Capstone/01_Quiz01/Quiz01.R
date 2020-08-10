library(tm)

setwd("~/Documentos/Data Science/Coursera/Course_10_Capstone_Project/Week01")

fileLink <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

download.file(fileLink, destfile = "capstData.zip")

unzip(zipfile = "capstData.zip")

# QUESTION 01 -------------------------------------------------------------

info <- file.info("final/en_US/en_US.blogs.txt")

sizeMb <- info$size/(1024*1024) # Size in MB

# QUESTION 02 -------------------------------------------------------------


twitter <- readLines(con <- file("final/en_US/en_US.twitter.txt"), 
                     encoding = "UTF-8", 
                     skipNul = TRUE)

length(twitter)

# QUESTION 03 -------------------------------------------------------------

# Blogs file

blogs <- file("final/en_US/en_US.blogs.txt", "r") # Open a connection

blogLines <- readLines(blogs) # Reads all text on single file

close(blogs) # Closes a conection

n <- nchar(blogLines)

summary(nchar(blogLines)) # Summary of amount of characters for each line

# News file

news <- file("final/en_US/en_US.news.txt","r")

newsLines <- readLines(news)

close(news)

summary(nchar(newsLines))

# Twitter file

twitter <- file("final/en_US/en_US.twitter.txt","r")

twitterLines <- readLines(twitter)

close(twitter)

summary(nchar(twitterLines))

# QUESTION 04 -------------------------------------------------------------

love <- length(grep("love", twitterLines))

hate <- length(grep("hate", twitterLines))

love / hate

# QUESTION 05 -------------------------------------------------------------

grep("biostats", twitterLines, value = T)

# QUESTION 06 -------------------------------------------------------------

grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitterLines)
