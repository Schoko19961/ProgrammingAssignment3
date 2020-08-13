rankhospital <- function(state, outcome, num = "best") {
  csv <-
    read.csv(file.path("datasource", "outcome-of-care-measures.csv"),
             colClasses = "character")
  
  rHospitals <- csv[csv$State == state,]
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
  #Exclude Hospitals with NA values
  rHospitals <- rHospitals[!is.na(rHospitals[location]),]
  #sort hospitals
  rHospitals <- rHospitals[order(rHospitals[,location], rHospitals[,2]),]
  if(is.numeric(num) & num <= length(rHospitals) & num > 0){
    rHospitals[num,2]
  }else if(num == 'best'){
    rHospitals[1,2]
  }else if(num == 'worst'){
    rHospitals[nrow(rHospitals),2]
  }else{
    NA
  }
}