pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  full.data <- data.frame()
  
    for (i in id) {
    
      if (i < 10) {
        
        path <- paste(directory, "/00", i, ".csv", sep = "") # Creates full file place
        
      } else if (i < 100) {
        
        path <- paste(directory, "/0", i, ".csv", sep = "") # Creates full file place
        
      }else {
        
        path <- paste(directory, "/", i, ".csv", sep = "") # Creates full file place
        
      }
      
      df <- read.csv(path) # Reads file
      
      full.data <- rbind(full.data, df)
    
  }
  
  
  pol <- full.data[pollutant]
  
  na.pos <- is.na(pol)
  
  mean(pol[!na.pos])
 
}

mean_pol <- pollutantmean(directory = "~/Google Drive/R/Learning/Johns-Hopkins/Course 02 - R Programming/Week 02/specdata/",
                          pollutant = "nitrate"
                          )
