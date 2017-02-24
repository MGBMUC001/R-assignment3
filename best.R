best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.string = "Not Available",  stringsAsFactors = FALSE)
  subdata <- subset(data, select=c(State, Hospital.Name,
                                   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                   Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  names(subdata) <- c("State", "Hospital.Name", "Heart.Attack", 
                      "Heart.Failure","Pneumonia")
  
  ## Check that state and outcome are valid
  state_data <- subset(subdata, State==state)
  if ( nrow(state_data)==0 ) {
    stop("invalid state")
  }
  if ( (outcome!="heart attack") & (outcome!="heart failure")
       & (outcome!="pneumonia") )   {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  if ( outcome=="heart attack")  {
    hos_rank <- state_data[ order(state_data$Heart.Attack, state_data$Hospital.Name), ]
  }
  else if ( outcome=="heart failure")  {
    hos_rank <- state_data[ order(state_data$Heart.Failure, state_data$Hospital.Name), ]
  }
  else  {
    hos_rank <- state_data[ order(state_data$Pneumonia, state_data$Hospital.Name), ]
  }
  best_hos <- hos_rank[[1,2]]
  return(best_hos)
}
getwd()
setwd("/users/Gudza-Mugabe/desktop")
setwd("prog_assignment3")
getwd()
best("MD", "heart attack")
best("NJ", "heart attack")
best("OH", "heart failure")
best("WA", "pneumonia")
best("NE", "diabetes")
best("AK", "pneumoniae")
best ("PW","heart failure")
best("FL", "heart failure")
  