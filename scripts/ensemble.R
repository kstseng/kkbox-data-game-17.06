library(xgboost)

##' get out-of-fold prediction
pred.train.df <- xgbcv$pred
colnames(pred.train.df) <- colnames(tmp)

pred_train <- as.vector(t(as.matrix(pred.train.df)))
user.train <- labels_train$user_id

##' select top 3 prediction for training 
train_pred_long <- data.frame(user_id = rep(user.train, each=ncol(train_pred)), title_id = mapping.table$ori, pred = pred_train)
max_prob_tr <- train_pred_long %>% 
  group_by(user_id) %>% 
  filter(pred == max(pred))
second_prob_tr <- train_pred_long %>% 
  arrange(user_id, -pred) %>% 
  group_by(user_id) %>% 
  filter(row_number() == 2)
third_prob_tr <- train_pred_long %>% 
  arrange(user_id, -pred) %>% 
  group_by(user_id) %>% 
  filter(row_number() == 3)

##' 
submission_ensemble_train <- train_tm %>% rename(title_id_tm = title_id) %>% 
  left_join(max_prob_tr %>% rename(title_id_1 = title_id, pred_1 = pred), by = "user_id") %>% 
  left_join(second_prob_tr %>% rename(title_id_2 = title_id, pred_2 = pred), by = "user_id") %>% 
  left_join(third_prob_tr %>% rename(title_id_3 = title_id, pred_3 = pred), by = "user_id")


##' magic number selection
mn1 <- seq(0.05, 0.2, 0.01)
mn2 <- seq(0.05, 0.2, 0.01)
magic.number.grid <- expand.grid(mn1, mn2)

score <- 0
for(i in 1:nrow(magic.number.grid)){
  print(i/nrow(magic.number.grid))
  # magic_number <- c(0.100, 0.12) #:0.28518  
  magic_number <- c(magic.number.grid[i, 1], magic.number.grid[i, 2])

  submission_ensemble_summary_train <- submission_ensemble_train %>% 
    mutate(title_pred = if_else(title_id_1 != as.integer(-99), title_id_1, 
                                if_else(pred_2 >= magic_number[1] & title_id_2 != as.integer(-99), title_id_2, 
                                        if_else(pred_3 >= magic_number[2] & title_id_3 != as.integer(-99), title_id_3, as.integer(-99))))) %>% 
    mutate(title_id = if_else(title_pred != as.integer(-99), as.character(title_pred), as.character(title_id_tm)))  
  
  tmp <- submission_ensemble_summary_train %>% 
    select(user_id, title_id) %>% 
    left_join(labels_train %>% rename(title_id_true = title_id), by = "user_id")
  score[i] <- sum(tmp$title_id == tmp$title_id_true)/nrow(tmp)
}

magic_number <- magic.number.grid[which(score == max(score)), ]
max(score)

##' for testing
strpadFunc <- function(submission){
  library(stringr)
  submission$user_id <- str_pad(submission$user_id, 8, pad = "0")
  submission$title_id <- str_pad(submission$title_id, 8, pad = "0")
  return(submission)
}

submission_ensemble_summary <- submission_ensemble %>% 
  mutate(title_pred = if_else(title_id_1 != as.integer(-99), title_id_1, 
                              if_else(pred_2 >= magic_number[1] & title_id_2 != as.integer(-99), title_id_2, 
                                      if_else(pred_3 >= magic_number[2] & title_id_3 != as.integer(-99), title_id_3, as.integer(-99))))) %>% 
  mutate(title_id = if_else(title_pred != -99, title_pred, title_id_tm))
submission_ensemble_summary_final <- submission_ensemble_summary %>% 
  select(user_id, title_id)
fwrite(strpadFunc(submission_ensemble_summary_final), "submission/submission_1st_solution.csv") #