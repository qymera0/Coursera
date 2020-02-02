
# 1 - PLOT 30 DAY MORTALITY -----------------------------------------------

outcome <- read.csv("~/Google Drive/R/Learning/Johns-Hopkins/Course 02 - R Programming/Week 04/Dataset/outcome-of-care-measures.csv", 
                    stringsAsFactors = FALSE)

outcome[ ,11] <- as.numeric(outcome[ ,11])

hist(outcome[ ,11])

place <- "~/Google Drive/R/Learning/Johns-Hopkins/Course 02 - R Programming/Week 04/Dataset/outcome-of-care-measures.csv"


# 2 - FIND THE BEST HOSPITAL IN A STATE -----------------------------------


open_hospitals <- function(place) {
  
  # Read and wrangle the outcome data
  
  df <- read.csv(place,
                 stringsAsFactors = FALSE)
  
  df <- df[ ,c(2, 7, 11, 17, 23)] # Select only the data used
  
  df[ ,c(3, 4, 5)] <- sapply(df[ ,c(3, 4, 5)], as.numeric)
  
  df[ ,2] <- as.factor(df[ ,2])
  
  possible.outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  colnames(df) <- c("Name", "State.abb", possible.outcomes)
  
  df
  
}

best <- function(state, outcome){
  
  options(warn = -1)
  
  possible.outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  # Read and wrangle the outcome data
  
  df <- open_hospitals(place)
  
  # Check that state and outcome is valid
  
  if (!any(state == df["State.abb"])) {stop("Invalid State!")}
 
  if (!any(outcome == possible.outcomes)) {stop("Invalid Outcome!")}
  
  # Look for Lowest Rate
  
  filtered <- df[df$State.abb == state, ]
  
  min.filtered <- which(filtered[ ,outcome] == min(filtered[ ,outcome], na.rm = TRUE))
  
  minimum.outcome <- filtered[min.filtered, "Name"]
  
  # Handling Ties
  
  ordered.outcome <- minimum.outcome[order(minimum.outcome)]
  
  ordered.outcome[1]
    
}

# Test Function best

best("TX", "heart attack")

best("TX", "heart failure")

best("MD", "heart attack")

best("MD", "pneumonia")

best("BB", "heart attack")

best("NY", "hert attack")


# 3 - RANKING HOSPITALS BY OUTCOME IN A STATE -----------------------------

rankhospital <- function(state, outcome, num = "best") {
  
  options(warn = -1)
  
  possible.outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  # Read and wrangle the outcome data
  
  df <- open_hospitals(place)
  
  # Check that state and outcome is valid
  
  if (!any(state == df["State.abb"])) {stop("Invalid State!")}
  
  if (!any(outcome == possible.outcomes)) {stop("Invalid Outcome!")}
  
  # Rnak Hospitals by outcome
  
  filtered <- df[df$State.abb == state, ]
  
  
  
}
  
  


