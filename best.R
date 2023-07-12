sheet <- read.csv("outcome-of-care-measures.csv", colClasses = "character",header = TRUE)

sheet[, 11] <- as.numeric(sheet[, 11])
sheet[,17] <- as.numeric(sheet[,17])
sheet[, 23] <- as.numeric(sheet[, 23])


x<-split(sheet,sheet$State)

best<-function(state,outcome){
  
  if (state %in% names(x)){
  target_state<-x[[state]]
  } else {
    stop('Invalid Syntax')
  }
  
  if (outcome=='heart attack'){
    n=11
  } else if (outcome=='heart failure'){
    n=17
  } else if (outcome=='pneumonia') {
    n=23
  } else {
    stop('Invalid Syntax')
  }
  
  sort_by<-target_state[order(target_state[,n]),]
  result<-sort_by[1,c(2,n)]
  result
  
}

