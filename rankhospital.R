rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Order the data by outcome rate followed by alphabetical order of hospital name
    ordered_outcome <- state_outcome[order(state_outcome[, colIndex], state_outcome[, 2]), ]
    
    ## Find the worst index value
    worst_index <- sum(!is.na(ordered_outcome[,colIndex]))
    
    ## Declare rank value for indexing purposes
    if(num == "best") {
        rank <- 1
    } else if( num == "worst") {
        rank <- worst_index
    } else {
        rank <- num
    }
    
    ## Return hospital name of choice
    ordered_outcome[rank, 2]
}