
# 00 LOADING PACKAGES -----------------------------------------------------

library(tidyverse)

# 01 READING FILES --------------------------------------------------------

setwd("C:/Users/bacos1/Documents/R/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

# setwd("/home/qymera0/Documentos/Data Science/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week04")

NEI <- readRDS("summarySCC_PM25.rds") # Main dataset

SCC <- readRDS("Source_Classification_Code.rds") # Description of fips


# 02 TOTAL EMISSION ACROSS TIME -------------------------------------------

str(NEI)

with(NEI, summary(Emissions))

with(NEI, tapply(Emissions, year, FUN = summary))

# Total amount of pollutant

totalAnnual <- sapply(with(NEI, split(Emissions, year)), FUN = sum)

years <- unique(NEI$year)

pts <- pretty(totalAnnual/1000000)

png(file = "plot1_1.png")

plot(years, totalAnnual/1000000, type = "l", lwd = 2, axes = FALSE,
     ylab = "",
     xlab = "")

mtext(text = "Pollutant in USA over years", side = 3, line = 2, adj = 0, cex = 1)

mtext(text = expression("Total tons of PM"[2.5]*" emissions"), side = 3, line = 1, adj = 0, cex = 0.8)

axis(1, at = years, labels = paste(years))

axis(2, at = pts, labels = paste(pts, "M", sep = ""));

dev.off()

# Remove outliers for box Plot

remove_outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs = 0.75, na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        x[x >= (qnt + H)] <- NA
        x
}

NEI$Emiss.clean <- unlist(sapply(with(NEI, 
                                      split(Emissions, year)), 
                                      remove_outliers),
                                use.names = FALSE)

# Box plot of pollutant

yaxis_points <- c(0, 0.2, 0.4, 0.6)

png(file = "plot1_2.png")

with(NEI, boxplot(Emiss.clean ~ year, na.rm = TRUE, frame.plot = FALSE))

mtext(text = "Pollutant variation in USA", side = 3, line = 2, adj = 0, cex = 1)

mtext(text = expression("PM"[2.5]*" Emissions"), side = 3, line = 1, adj = 0, cex = 0.8)

title(sub = "outliers removed",
      cex.sub = 0.7,
      adj = 0)

dev.off()

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

g4 <- ggplot(NEIVehCit, aes(x = year, y = VehAnTotal, fill = SCC.Level.Two))

g4 + geom_bar(stat = "identity", position = "stack") +
  facet_grid(~fips) + 
  scale_fill_manual(values = c("dodgerblue", "dodgerblue4"),
                    name = "") +
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
        legend.title = element_text(colour = "antiquewhite4")
  )
  

dev.off()

g4 + geom_bar(stat = "identity", position = "stack") +
  facet_grid(~fips) +
 theme(legend.text = element_text(colour = "grey"))
