## Function "best" takes as input a state and mortality outcome of interest and returns the 
## Hospital name that has the best mortality rate for the outcome in question.

best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    if(!is.element(state,state.abb)) {
        stop("invalid state")
    }
    
    ## Check that outcome is valid
    outcome_list <- c("heart attack", "heart failure", "pneumonia")
    if(!is.element(outcome, outcome_list)) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    for(i in c(11, 17, 23)) {
        outcome_data[, i] <- as.numeric(outcome_data[, i])
    }

    if(outcome == "heart attack") {
        colIndex <- 11
    } else if(outcome == "heart failure") {
        colIndex <- 17
    } else {
        colIndex <- 23
    }
    
    ## Subsets data frame to state of interest
    state_outcome <- outcome_data[which(outcome_data[, 7] == state),]
    
    ## Finds the minimum outcome value of interest for that state
    min_outcome <- min(state_outcome[,colIndex], na.rm = TRUE)
    
    ## Returns the names of the Hospital
    leading_hospital <- state_outcome[which(state_outcome[, colIndex] == min_outcome), 2]
    
    ## Tie breaker in the event of multiple hospitals
    ord <- order(leading_hospital)
    first_hospital_index <- ord[1]
    leading_hospital[first_hospital_index] 
}