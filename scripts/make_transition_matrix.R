library(markovchain)
library(dplyr)
event.factor <- factor(x=event_all$title_id)
event.tpm <- createSequenceMatrix(event.factor, toRowProbs = TRUE)

##' build n x 2 matrix 
library(zoo)
label_all <- rbind(label_train, sample)
event_all <- rbind(event_train, event_test)
m <- list()
for(i in 1:nrow(label_all)){
  print(i/nrow(label_all))
  user.i <- label_all$user_id[i]
  data.i <- event_all[user_id == user.i]
  
  title_id.seq <- c(data.i$title_id, label_train[user_id == user.i] %>% .$title_id)
  
  if(nrow(data.i) > 1){
    m[[i]] <- rollapply(title_id.seq, 2, by = 1, c) %>% as.data.frame()
  }else{
    m[[i]] <- NULL
  }
}
sequence.event.matrix <- plyr::rbind.fill(m)
event.tpm.m <- createSequenceMatrix(sequence.event.matrix, toRowProbs = TRUE)