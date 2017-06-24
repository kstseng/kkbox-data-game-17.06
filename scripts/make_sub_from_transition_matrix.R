##' transition matrix prediction to training data
train_tm <- label_train
for(i in 1:nrow(train_tm)){
  print(i/nrow(train_tm))
  event.i <- event_train %>% filter(user_id == train_tm$user_id[i])  
  title.last <- tail(event.i, 1) %>% .$title_id
  if(title.last %in% row.title){
    selected <- event.tpm.m[which(row.title == title.last), ]
    title.max <- names(which(selected == max(selected)))
    train_tm$title_id[i] <- title.max[1]
  }else{
    train_tm$title_id[i] <- title.last
  }
}

##' transition matrix prediction to testing data
row.title <- rownames(event.tpm.m)
submission.tm <- sample
for(i in 1:nrow(sample)){
  print(i/nrow(sample))
  event.i <- event_test %>% filter(user_id == submission.tm$user_id[i])  
  title.last <- tail(event.i, 1) %>% .$title_id
  if(title.last %in% row.title){
    selected <- event.tpm.m[which(row.title == title.last), ]
    title.max <- names(which(selected == max(selected)))
    submission.tm$title_id[i] <- title.max[1]
  }else{
    submission.tm$title_id[i] <- title.last
  }
}
submission.tm <- strpadFunc(submission.tm)