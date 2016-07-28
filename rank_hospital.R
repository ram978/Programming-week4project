rankall <- function(outcome, num = "best") {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  x <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else if (outcome == "pneumonia") {
    23
  } else {
    stop("invalid outcome")
  }
  
  outcome_data <- outcome_data[, c(7, 2, x)]
  outcome_data[, 3] <- suppressWarnings(as.numeric(outcome_data[, 3]))
  outcome_data <- outcome_data[order(outcome_data[1], outcome_data[3], outcome_data[2]), ]
  outcome_data <- outcome_data[!is.na(outcome_data[3]), ]
  outcome_split_data <- split(outcome_data, outcome_data[1])
  
  if (num == "best") {
    table <- lapply(outcome_split_data, function(q) { q[1,] })
  } else if (num == "worst") {
    table <- lapply(outcome_split_data, function(q) { q[nrow(q),] })
  } else {
    table <- lapply(outcome_split_data, function(q) { q[num,] })
  }
  
  hosp_state <- as.data.frame(do.call(rbind, table))
  hosp_state[,1] <- row.names(hosp_state)
  hosp_state <- hosp_state[, 2:1]
  colnames(hosp_state) <- c("hospital", "state")
  return(hosp_state)
}
