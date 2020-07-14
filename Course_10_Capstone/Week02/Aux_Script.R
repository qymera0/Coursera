
library(janeaustenr)

# LOAD DATA ---------------------------------------------------------------


blog <- readLines("dataset/final/en_US/en_US.blogs.txt", 
                 warn = F,
                 skipNul = T)

news <- readLines("dataset/final/en_US/en_US.news.txt",
                 warn = F,
                 skipNul = T)

twitter <- readLines(file("dataset/final/en_US/en_US.twitter.txt",
                         open = "rb"),
                    warn = F,
                    skipNul = T)

rawTidy <- tidy(rawData)

t <- lexicon::profanity_alvarez
