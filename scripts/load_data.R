library(data.table)
library(dplyr)
#library(beepr)
event_train <- fread("data/assets/events_train.csv")
event_test <- fread("data/assets/events_test.csv")
event_train$date <- as.POSIXct(event_train$time, origin="1970-01-01",tz="Asia/Taipei")
event_test$date <- as.POSIXct(event_test$time, origin="1970-01-01",tz="Asia/Taipei")

label_train <- fread("data/assets/labels_train.csv")
sample <- fread("data/assets/sample.csv")

##'
event_train <- event_train[order(event_train$user_id, event_train$time), ]
event_test <- event_test[order(event_test$user_id, event_test$time), ]
event_all <- rbind(event_train, event_test)

strpadFunc <- function(submission){
  library(stringr)
  submission$user_id <- str_pad(submission$user_id, 8, pad = "0")
  submission$title_id <- str_pad(submission$title_id, 8, pad = "0")
  return(submission)
}
