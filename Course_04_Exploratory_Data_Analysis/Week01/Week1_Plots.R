
# 0 INITIATE CODE ---------------------------------------------------------

library(readr)
library(dplyr)

setwd("D:/Samuel/Meus Documentos/Google Drive/R/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week01")

# 1 DOWNLOAD DATA ---------------------------------------------------------

filelink <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

download.file(url = filelink, destfile = "power.zip")

# 2 LOAD FILE -------------------------------------------------------------

# Load, convert Date columns to date type and select data

powerData <- read_delim("power.zip", ";", escape_double = FALSE, 
                        col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                        trim_ws = TRUE, na = "?") %>%
             filter(Date >= "2007-02-01" & Date <= "2007-02-02")


# 3 PLOT 01 ---------------------------------------------------------------

setwd("D:/Samuel/Meus Documentos/Google Drive/R/Learning/Johns_Hopkins_Coursera/Course_04_Exploratory_Data_Analysis/Week01/Plots")

png(file = "plot1.png")

with(powerData, hist(Global_active_power, 
                     col = "red", 
                     xlab = "Global Active Power (kilowatts)",
                     main = "Global Active Power"))

dev.off()


# 4 PLOT 2 ----------------------------------------------------------------

#Combine Date and Time at one variable

powerData$dateTime <- with(powerData, paste(Date, Time)) %>%
        as.POSIXct()

png(file = "plot2.png")

with(powerData, plot(dateTime, Global_active_power, 
                     type = "l", 
                     ylab = "Global Active Power (kilowatts)",
                     xlab = ""))

dev.off()
 

# 5 PLOT 3 ----------------------------------------------------------------

png(filename = "plot3.png")

with(powerData, {
        plot(Sub_metering_1~dateTime, type="l",
             ylab="Global Active Power (kilowatts)", xlab="")
        
        lines(Sub_metering_2~dateTime,col='Red')
        
        lines(Sub_metering_3~dateTime,col='Blue')
})

legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()


# 6 PLOT 4 ----------------------------------------------------------------

png(filename = "plot4.png")

par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))

with(powerData, {
        plot(Global_active_power ~ dateTime, type = "l", 
             ylab = "Global Active Power (kilowatts)", xlab = "")
        
        plot(Voltage ~ dateTime, type = "l", 
             ylab = "Voltage (volt)", xlab = "")
        
        plot(Sub_metering_1 ~ dateTime, type = "l", 
             ylab = "Global Active Power (kilowatts)", xlab = "")
        
        lines(Sub_metering_2 ~ dateTime,col = 'Red')
        
        lines(Sub_metering_3 ~ dateTime,col = 'Blue')
        
        legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, bty = "n",
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        plot(Global_reactive_power ~ dateTime, type = "l", 
             ylab = "Global Rective Power (kilowatts)",xlab = "")
})

dev.off()


