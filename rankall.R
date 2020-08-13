rankall <- function(outcome, num = "best") {
  rHospitals <-
    read.csv(file.path("datasource", "outcome-of-care-measures.csv"),
             colClasses = "character")
  
  location <- NULL
  
  if (nrow(rHospitals) == 0) {
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
  rHospitals[, location] <-
    as.numeric(rHospitals[, location])
  rHospitals <- rHospitals[!is.na(rHospitals[location]),]
  #split Hopsitals based on State
  splitted <- split(rHospitals, rHospitals$State)
  # Sort values in each "category" & get requested Hospital, turn list back to dataframe
  values <- as.data.frame(sapply(splitted,function(data){
    ordered <- data[order(data[, location], data[, 2], na.last = T), ]
    findValueByNum(ordered,num)
    }))
  # Inverse columns and rows
  returnVal <- data.frame(t(values))[,c(2,7)]
  #Set column names
  colnames(returnVal) <- c('hospital', 'state')
  #Set state names (State names appear as rownames)
  returnVal[,'state'] <- row.names(returnVal)
  returnVal
}

findValueByNum <- function(data, num){
  if (is.numeric(num) & num <= length(data) & num > 0) {
    data[num, ]
  } else if (num == 'best') {
    data[1, ]
  } else if (num == 'worst') {
    data[nrow(data), ]
  } else{
    NA
  }
}