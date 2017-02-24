rankall <- function(outcome, num = "best") {
  rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank 30-day death rate
    if ( outcome=="heart attack")  {
      hos_rank <- state_data[ order(state_data$Heart.Attack, state_data$Hospital.Name), ]
      hos_rank_naomit <- subset(hos_rank, !is.na(hos_rank$Heart.Attack) )
    }
    else if ( outcome=="heart failure")  {
      hos_rank <- state_data[ order(state_data$Heart.Failure, state_data$Hospital.Name), ]
      hos_rank_naomit <- subset(hos_rank, !is.na(hos_rank$Heart.Failure) )
    }
    else  {
      hos_rank <- state_data[ order(state_data$Pneumonia, state_data$Hospital.Name), ]
      hos_rank_naomit <- subset(hos_rank, !is.na(hos_rank$Pneumonia) )
    }
    n <- nrow(hos_rank_naomit)
    hos_rank_naomit$Rank <- 1:n
    if (num == "best")  {
      return(hos_rank_naomit[[1,2]])
    }
    else if (num == "worst")  {
      return( hos_rank_naomit[[n,2]] )
    } 
    else if ( (num>=1)&(num<=n) )  {
      return( hos_rank_naomit[[num,2]] )
    }
    else  {
      return(NA)
    }
    
  }
  allstates <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI",
                 "IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO",
                 "MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR",
                 "PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI",
                 "WV","WY")
  hospitals <- vector()
  for (i in 1:54)  {
    hospitals[i] <- rankhospital(allstates[i], outcome=outcome, num=num) 
  }
  results <- data.frame(hospitals,allstates)
  names(results) <- c("hospital","state")
  rownames(results) <- allstates
  return(results)
}
head(rankall("pneumonia",6))
head(rankall("heart attack", 10))
tail(rankall("heart failure",8))
tail(rankall("heart failure", 2))
head(rankall("heart attack",20),10)
tail(rankall("heart attack", 5))
tail(rankall("pneumonia",2))
