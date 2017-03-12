rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome are valid
    outcome_list <- c("heart attack", "heart failure", "pneumonia")
    if(!is.element(outcome, outcome_list)) {
          stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    for(i in c(11, 17, 23)) {
        outcome_data[, i] <- as.numeric(outcome_data[, i])
    }
    
    ## Define the column index based on outcome of interest
    if(outcome == "heart attack") {
          colIndex <- 11
    } else if(outcome == "heart failure") {
          colIndex <- 17
    } else {
          colIndex <- 23
    }
    
    ## Create an empty data frame to store hospital by state
    all_states <- data.frame(matrix(NA, nrow = 50, ncol = 2))
    names(all_states) <- c("hospital", "state")
    
    ## Create list of sorted list of states plus DC
    states <- sort(c(state.abb, "DC", "VI"))
    
    for(i in 1:length(states)) {
        ## Subsets data frame to state of interest
        state_outcome <- outcome_data[which(outcome_data[, 7] == states[i]),]
    
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
        
        all_states[i,] <- c(ordered_outcome[rank, 2], states[i])
    }
    
    ## Return a data frame with the hospital names and the ## (abbreviated) state name
    all_states
}