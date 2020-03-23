
# 00 LOADING PACKAGES -----------------------------------------------------

library(data.table)
library(tidyverse)

# 01 READING FILES --------------------------------------------------------

setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

NEI <- readRDS("summarySCC_PM25.rds")

SCC <- readRDS("Source_Classification_Code.rds")


# 02 TOTAL EMISSION ACROSS TIME -------------------------------------------

str(NEI)

with(NEI, summary(Emissions))

with(NEI, tapply(Emissions, year, FUN = summary))

# Total Amount of Pollutant

totalAnnual <- sapply(with(NEI, split(Emissions, year)), FUN = sum)

years <- unique(NEI$year)

pts <- pretty(totalAnnual/1000000)

plot(years, totalAnnual/1000000, type = "l", lwd = 2, axes = FALSE,
     ylab = "",
     xlab = "")

mtext(text = "Pollutant in USA over years", side = 3, line = 2, adj = 0, cex = 1)

mtext(text = expression("Total Tons of PM"[2.5]*" Emissions"), side = 3, line = 1, adj = 0, cex = 0.8)

axis(1, at = years, labels = paste(years))

axis(2, at = pts, labels = paste(pts, "M", sep = ""));

# Remove Outliers

remove_outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs = 0.75, na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        x[x >= (qnt + H)] <- NA
        x
}

NEI$Emiss.clean <- unlist(sapply(with(NEI, split(Emissions, year)), remove_outliers),
                          use.names = FALSE)

# Box Plot of data

yaxis_points <- c(0, 0.2, 0.4, 0.6)

with(NEI, boxplot(Emiss.clean ~ year, na.rm = TRUE, frame.plot = FALSE))

mtext(text = "Pollutant variation in USA", side = 3, line = 2, adj = 0, cex = 1)

mtext(text = expression("PM"[2.5]*" Emissions"), side = 3, line = 1, adj = 0, cex = 0.8)

title(sub = "Outliers removed",
      cex.sub = 0.7,
      adj = 0)
