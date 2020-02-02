corr <- function(directory, threshold = 0){
  
  full.data <- vector()
  
  for (i in 1:length(list.files(directory))) {
    
    if (i < 10) {
      
      path <- paste(directory, "/00", i, ".csv", sep = "") # Creates full file place
      
    } else if (i < 100) {
      
      path <- paste(directory, "/0", i, ".csv", sep = "") # Creates full file place
      
    }else {
      
      path <- paste(directory, "/", i, ".csv", sep = "") # Creates full file place
      
    }
    
    df <- read.csv(path) # Reads file
    
    df$Date <- NULL
    
    df$ID <- NULL
    
    if (length(complete.cases(df)[complete.cases(df)]) > threshold) {
      
      correlation <- cor(df[complete.cases(df), ])[2,1]
      
      full.data <- rbind(full.data, correlation)
        
    }
    
  }
  
  return(full.data)
  
}

cr <- corr(directory = "~/Google Drive/R/Learning/Johns-Hopkins/Course 02 - R Programming/Week 02/specdata/", 2000)

n <- length(cr)                

cr <- corr(directory = "~/Google Drive/R/Learning/Johns-Hopkins/Course 02 - R Programming/Week 02/specdata/", 1000)              

cr <- sort(cr)

print(c(n, round(cr, 4)))
