complete <- function(directory, id = 1:332){
  
  full.data <- data.frame()
  
  compcases <- data.frame()
  
  for (i in id) {
    
    if (i < 10) {
      
      path <- paste(directory, "/00", i, ".csv", sep = "") # Creates full file place
      
    } else if (i < 100) {
      
      path <- paste(directory, "/0", i, ".csv", sep = "") # Creates full file place
      
    }else {
      
      path <- paste(directory, "/", i, ".csv", sep = "") # Creates full file place
      
    }
    
   df <- read.csv(path) # Reads file
    
   compcases[1,1] <- i
   
   compcases[1,2] <- length(complete.cases(df)[complete.cases(df)])
    
   full.data <- rbind(full.data, compcases)
   
  }
  
  names(full.data) <- c("id", "nobs")
  
  return(full.data)
}

cases <- complete(directory = "~/Google Drive/R/Learning/Johns-Hopkins/Course 02 - R Programming/Week 02/specdata/",
                  id = 3)


