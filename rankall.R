sheet <- read.csv("outcome-of-care-measures.csv", colClasses = "character",header = TRUE)
sheet[, 11] <- as.numeric(sheet[, 11])
sheet[,17] <- as.numeric(sheet[,17])
sheet[, 23] <- as.numeric(sheet[, 23])
x<-split(sheet,sheet$State)


rankall<-function(outcome,num){
  
  if (outcome=='heart attack'){
    n=11
  } else if (outcome=='heart failure'){
    n=17
  } else if (outcome=='pneumonia') {
    n=23
  } else {
    stop('Invalid Syntax')
  }
  
  
  
  
  for (i in names(x)){
    all_states<-x[[i]]
    
    if (num=='worst'){
      num=length(all_states[,n])
    } else if (num=='best'){
      num=1
    }
    rank<-rbind(rank,all_states[order(all_states[,n]),][num,c(2,7,n)])

  }
  
  num
}