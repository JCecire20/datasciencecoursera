### Part 3: Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df   <- as.data.frame(cbind(data[, 2],      # hospital
                              data[, 7],      # state
                              data[, 11],     # heart attack
                              data[, 17],     # heart failure
                              data[, 23]),    # pneumonia
                        stringsAsFactors = FALSE)
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% df[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    si <- which(df[, "state"] == state)
    ts <- df[si, ]                     # extracting dataframe for the called state
    ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
    ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
    output <- ts[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      si <- which(df[, "state"] == state)
      ts <- df[si, ]    
      ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
      ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
      output <- ts[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}
