best <- function(state, outcome){
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header =TRUE, na.strings = "Not Available")
  outcome_need <- outcome_data[, c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  cnames <-c("Hospital.Name", "State", "Pneumonia", "Heart.Failure", "Heart.Attack")
  colnames(outcome_need)<-cnames
  names(outcome_need)
  ## Check that state and outcome are valid
  if(!(state %in% outcome_need$State)){
    stop("invalid state")
  }
  
  if (! outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalid outcome")
  } 
  if(outcome == "heart attack") {
    x <-5
  } else if(outcome == "heart failure") {
    x <-4
  } else if(outcome == "pneumonia") {
    x <-3
  }#outcome_heart_attack <- outcome_need[, c("Hospital.Name", "State", "Heart.Failure")]
    state_set=subset(outcome_need, outcome_need$State == state)
    View(state_set)
    state_set[,x] <- as.numeric(state_set[,x])
    min_of_state <- min(state_set[,x], na.rm = TRUE)
    result_set <- subset(state_set,state_set[,x]  == min_of_state)
    result_set$Hospital.Name
  
}
