best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  data <- subset(data, data[,2] == state) ##subsets data based on state
  ##line 32 prevents issue where min function returns wrong min due to character
  ##it also prevents a rounding error in the as.numeric character conversion
  min_mort <- format(min(as.numeric(data[,3])), nsmall = 1)
  data <- subset(data, data[,3] == as.character(min_mort)) ##subsets data to min values 
  results <- data[,1]
  
  if (length(results) > 1) {
    sort(results)
    results <- results[1]
  }

  print(results)
}