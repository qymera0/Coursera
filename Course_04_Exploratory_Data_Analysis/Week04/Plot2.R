# 00 LOADING PACKAGES -----------------------------------------------------

library(tidyverse)

# 01 READING FILES --------------------------------------------------------

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

# setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

NEI <- readRDS("summarySCC_PM25.rds") # Main dataset

SCC <- readRDS("Source_Classification_Code.rds") # Description of fips

# 03 BALTIMORE CITY TOTAL -------------------------------------------------

# Select only data from Baltimore City

NEIBalt <- subset(NEI, fips=="24510")

# Total Amount of Pollutant

totAnBalt <- sapply(with(NEIBalt, split(Emissions, year)), FUN = sum)

years <- unique(NEIBalt$year)

pts <- pretty(totAnBalt/1000000)

png(file = "plot2_1.png")

plot(years, totAnBalt/1000000, type = "l", lwd = 2, axes = FALSE,
     ylab = "",
     xlab = "")

mtext(text = "Pollutant in Baltimore over years", side = 3, line = 2, adj = 0, cex = 1)

mtext(text = expression("Total tons of PM"[2.5]*" emissions"), side = 3, line = 1, adj = 0, cex = 0.8)

axis(1, at = years, labels = paste(years))

axis(2, at = pts, labels = paste(pts, "M", sep = ""));

dev.off()

# Remove Outliers

remove_outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs = 0.75, na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        x[x >= (qnt + H)] <- NA
        x
}

NEIBalt$Emiss.clean <- unlist(sapply(with(NEIBalt, split(Emissions, year)), remove_outliers),
                              use.names = FALSE)

# Box Plot of data

yaxis_points <- c(0, 0.2, 0.4, 0.6)

png(file = "plot2_2.png")

with(NEIBalt, boxplot(Emiss.clean ~ year, na.rm = TRUE, frame.plot = FALSE))

mtext(text = "Pollutant variation in Baltimore", side = 3, line = 2, adj = 0, cex = 1)

mtext(text = expression("PM"[2.5]*" emissions"), side = 3, line = 1, adj = 0, cex = 0.8)

title(sub = "outliers removed",
      cex.sub = 0.7,
      adj = 0)

dev.off()
