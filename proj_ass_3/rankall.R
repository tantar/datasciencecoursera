rankall <- function(outcome, num = "best") {
  ## Read outcome
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that outcome is valid
  valid_states <- unique(outcome_data[,7]) ##saves vector of valid states
  
  if (outcome == "heart attack") { ##determines if valid outcome was entered
    all_data <- outcome_data[c(2,7,11)]
    ##the following code was required to allow min function to be used on datadata
    all_data <- as.matrix(subset(all_data, (all_data[3] != "Not Available")))
  }
  else if (outcome == "heart failure") {
    all_data <- outcome_data[c(2,7,17)]
    all_data <- as.matrix(subset(all_data, (all_data[3] != "Not Available")))
  }
  else if (outcome == "pneumonia") {
    all_data <- outcome_data[c(2,7,23)]
    all_data <- as.matrix(subset(all_data, (all_data[3] != "Not Available")))
  }
  else {
    stop("invalid outcome")
  }
  results <- data.frame()
  for (i in 1:53) {
    data <- subset(all_data, all_data[,2] == valid_states[i]) ##subsets state data
    data <- data[order(as.numeric(data[,3]),data[,1]),] ##orders based on rate
    data <- cbind(data, 1:nrow(data)) # adds rank column
    colnames(data) <- c("hospital", "state", "rate", "rank") #adds names
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "worst") {
      state_result <- subset(data, data[,4] == nrow(data))
      
    } else if (num == "best") {
      state_result <- subset(data, data[,4] == 1)
      
    } else if (num > nrow(data)) {
      state_result <- c(NA, valid_states[i])
      
    }  else {
      state_result <- subset(data, data[,4] == num)
    }
    results <- rbind(results, state_result)
  }
  results <- results[,1:2]
  rownames(results) <- results[,2]
  return(results)
}