
# 00 LOADING PACKAGES -----------------------------------------------------

library(tidyverse)

# 01 READING FILES --------------------------------------------------------

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

# setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

NEI <- readRDS("summarySCC_PM25.rds") # Main dataset

SCC <- readRDS("Source_Classification_Code.rds") # Description of fips

# 07 COMPARE BALTIMORE AND LA ---------------------------------------------

sccVeh <- unique(SCC[grep("Mobile.*Vehicles", SCC$EI.Sector), ]$SCC) # Unique SCC lines that have vehicles in description

NEIVeh <- subset(NEI, SCC %in% sccVeh)

NEIVehCit <- NEIVeh %>%
        filter(fips == "24510" | fips == "06037") %>%
        left_join(SCC,by = "SCC") %>%
        mutate(year = as.factor(year),
               fips = case_when(fips == "06037" ~ "LA",
                                fips == "24510" ~ "Baltimore")) %>%
        group_by(fips, year, SCC.Level.Two, SCC.Level.Three) %>%
        summarise(VehAnTotal = sum(Emissions))

png(file = "plot6.png")

g4 <- ggplot(NEIVehCit, aes(x = year, y = VehAnTotal, fill = SCC.Level.Two ))

g4 + geom_bar(stat = "identity", position = "stack") +
        facet_grid(~fips) + 
        scale_fill_manual(values = c("dodgerblue", "dodgerblue4"),
                          name = "Type of Vehicles") +
        labs(title = "Comparison of Vehicles pollutant over the years",
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
              plot.subtitle = element_text(colour = "antiquewhite4"),
              strip.text = element_text(colour = "antiquewhite4"),
              legend.text = element_text(colour = "antiquewhite4"),
              legend.title = element_text(colour = "antiquewhite4")) 

dev.off()
