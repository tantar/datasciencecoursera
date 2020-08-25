rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  valid_states <- unique(outcome_data[,7]) ##saves vector of valid states
  
  if (sum(state == valid_states) != 1){ ##determines if valid state was entered
    stop("invalid state")
  }
  
  if (outcome == "heart attack") { ##determines if valid outcome was entered
    data <- outcome_data[c(2,7,11)]
    ##the following code was required to allow min function to be used on datadata
    data <- as.matrix(subset(data, (data[3] != "Not Available")))
  }
  else if (outcome == "heart failure") {
    data <- outcome_data[c(2,7,17)]
    data <- as.matrix(subset(data, (data[3] != "Not Available")))
  }
  else if (outcome == "pneumonia") {
    data <- outcome_data[c(2,7,23)]
    data <- as.matrix(subset(data, (data[3] != "Not Available")))
  }
  else {
    stop("invalid outcome")
  }
  
  data <- subset(data, data[,2] == state) ##subsets data based on state
  data <- cbind(data[,1], data[,3]) ##removes state column
  data <- data[order(as.numeric(data[,2]),data[,1]),] ##orders based on rate
  data <- cbind(data, 1:length(data[,1]))
  colnames(data) <- c("Hospital.Name", "Rate", "Rank")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if (num == "worst") {
    results <- subset(data, data[,3] == nrow(data))
    
  } else if (num == "best") {
    results <- subset(data, data[,3] == 1)
    
  } else if (num > nrow(data)) {
      results <- c(NA)
      
  }  else {
    results <- subset(data, data[,3] == num)
  }
  return(results[1])
}