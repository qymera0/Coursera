
# 00 LOADING PACKAGES -----------------------------------------------------

library(tidyverse)

# 01 READING FILES --------------------------------------------------------

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

# setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

NEI <- readRDS("summarySCC_PM25.rds") # Main dataset

SCC <- readRDS("Source_Classification_Code.rds") # Description of fips

# 06 VEHICLES IN BALTMORE -------------------------------------------------

sccVeh <- unique(SCC[grep("Mobile.*Vehicles", SCC$EI.Sector), ]$SCC) # Unique SCC lines that have vehicles in description

NEIVeh <- subset(NEI, SCC %in% sccVeh)

NEIVehgrp <- NEIVeh %>%
        filter(fips == "24510") %>%
        left_join(SCC,by = "SCC") %>%
        mutate(year = as.factor(year)) %>%
        group_by(year, SCC.Level.Two, SCC.Level.Three) %>%
        summarise(VehAnTotal = sum(Emissions))

png(file = "plot5.png")

g3 <- ggplot(NEIVehgrp, aes(x = year, y = VehAnTotal, fill = SCC.Level.Two ))

g3 + geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("dodgerblue", "dodgerblue4"),
                          name = "Type of Vehicles") +
        labs(title = "Vehicles pollutant in Baltimore over the years",
             subtitle = expression("Total of tons of PM"[2.5]*" emissions"),
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
