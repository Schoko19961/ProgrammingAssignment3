best <- function(state, outcome) {
  csv <-
    read.csv(file.path("datasource", "outcome-of-care-measures.csv"),
             colClasses = "character")
  
  relevantHospitals <- csv[csv$State == state,]
  location <- NULL
  
  if (nrow(relevantHospitals) == 0) {
    stop("invalid state")
  }
  
  if (outcome == 'heart attack') {
    location <- 11 #11 = HeartAttack
  } else if (outcome == 'heart failure') {
    location <- 17 #17 = HeartFailure
  } else if (outcome == 'pneumonia') {
    location <- 23 #23 = Pneumonia
  } else{
    stop("invalid outcome")
  }
  
  #cast relevant column to numeric
  relevantHospitals[, location] <-
    as.numeric(relevantHospitals[, location])
  #find hospitals with minimum Value
  minimum <- findMinValues(relevantHospitals, location)[2]
  #sort by name
  minimum <- minimum[order("Hospital.Name"), ]
  #return first element
  minimum[1]
}

findMinValues <- function(data, location) {
  minVal <- min(data[, location], na.rm = T)
  data[!is.na(data[location]) & data[location] == minVal, ]
}