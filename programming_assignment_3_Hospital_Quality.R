### Programming Assignment 3: Hospital Quality


### PART 1: Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11], xlab= "Deaths", main = "30-Day Mortality Rates from Heart Attack")


### Part 2: Finding the best hospital in a state
best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  df   <- as.data.frame(cbind(data[, 2],       # hospital
                              data[, 7],       # state
                              data[, 11],      # heart attack
                              data[, 17],      # heart failure
                              data[, 23]),     # pneumonia
                        stringsAsFactors = FALSE)
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% df[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    st <- which(df[, "state"] == state)
    ts <- df[st, ]                            # extracting data for the called state
    hos <- as.numeric(ts[, eval(outcome)])
    min_val <- min(hos, na.rm = TRUE)
    result  <- ts[, "hospital"][which(hos == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}


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


### Part 4: Ranking hospitals in all states

rankall <- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df   <- as.data.frame(cbind(data[, 2],      # hospital
                              data[, 7],      # state
                              data[, 11],     # heart attack
                              data[, 17],     # heart failure
                              data[, 23]),    # pneumonia
                        stringsAsFactors = FALSE)
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  df[, eval(outcome)] <- as.numeric(df[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    by_state <- with(df, split(df, state))
    ordered  <- list()
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                           by_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      by_state <- with(df, split(df, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      by_state <- with(df, split(df, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(output)
}
