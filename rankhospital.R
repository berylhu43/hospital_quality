sheet <- read.csv("outcome-of-care-measures.csv", colClasses = "character",header = TRUE)
sheet[, 11] <- as.numeric(sheet[, 11])
sheet[,17] <- as.numeric(sheet[,17])
sheet[, 23] <- as.numeric(sheet[, 23])
x<-split(sheet,sheet$State)


rankhospital<-function(state,outcome,num){
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
  
  compare<-target_state[,n]
  
  if (num=='worst'){
    num=length(compare)
  } else if (num=='best'){
    num=1
  }
  
  sort_by<-target_state[order(compare, na.last = NA),]
  hospital_rank<-sort_by[c(1:num),c(2,n)]
  hospital_rank
}



