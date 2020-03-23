
# 00 LOADING PACKAGES -----------------------------------------------------

library(tidyverse)

# 01 READING FILES --------------------------------------------------------

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

# setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

NEI <- readRDS("summarySCC_PM25.rds") # Main dataset

SCC <- readRDS("Source_Classification_Code.rds") # Description of fips

# 05 COAL COMBUSTION RELATED SOURCES --------------------------------------

sccCoal <- unique(SCC[grep("Fuel Comb.*Coal", SCC$EI.Sector), ]$SCC) # Unique SCC lines that have coal in description

NEICoal <- subset(NEI, SCC %in% sccCoal)

table(NEICoal$type)

NEICoaltgrp <- NEICoal %>%
        mutate(type = factor(type, levels = c("NONPOINT", "POINT")),
               year = as.factor(year)) %>%
        group_by(type, year) %>%
        summarise(CoalAnTotal = sum(Emissions))

png(file = "plot4.png")

g2 <- ggplot(NEICoaltgrp, aes(x = year, y = CoalAnTotal/1000000, fill = type ))

g2 + geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("dodgerblue", "dodgerblue4"),
                          name = "Source") +
        labs(title = "Coal pollutant over the years",
             subtitle = expression("Total in millions of tons of PM"[2.5]*" emissions"),
             y = "",
             x = "Year") +
        theme_minimal() + 
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "antiquewhite4"),
              axis.title = element_text(colour = "antiquewhite4"),
              axis.text = element_text(colour = "antiquewhite4"),
              plot.title = element_text(colour = "antiquewhite4"),
              plot.subtitle = element_text(colour = "antiquewhite4")) 

dev.off()
