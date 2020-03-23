
# 00 LOADING PACKAGES -----------------------------------------------------

library(tidyverse)

# 01 READING FILES --------------------------------------------------------

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

# setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

NEI <- readRDS("summarySCC_PM25.rds") # Main dataset

SCC <- readRDS("Source_Classification_Code.rds") # Description of fips

# 04 BALTIMORE CITY BY POLLUTANT ------------------------------------------

NEIBaltgrp <- NEIBalt %>%
        mutate(type = factor(type, levels = c("ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")),
               year = as.factor(year)) %>%
        group_by(type, year) %>%
        summarise(BaltAnTotal = sum(Emissions))

png(file = "plot3.png")

g <- ggplot(NEIBaltgrp, aes(x = year, y = BaltAnTotal))

g + geom_bar(stat = "identity", fill = "dodgerblue4") + 
        facet_grid(~ type) +
        labs(title = "Pollutant in Baltimore over the years",
             subtitle = expression("Total tons of PM"[2.5]*" emissions"),
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
              plot.subtitle = element_text(colour = "antiquewhite4"),
              strip.text = element_text(colour = "antiquewhite4"))

dev.off()