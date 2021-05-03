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
