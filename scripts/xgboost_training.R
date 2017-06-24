library(xgboost)
df.sample <- df.trsf.train


##' pre-processing to fit into xgboost function
title_id.cnt <- df.sample$title_id %>% table() %>% sort(decreasing=T)
not.rare.title_id <- names(title_id.cnt) %>% head(40)
title_id.vec <- df.sample$title_id %>% as.character()
title_id.vec[!title_id.vec %in% not.rare.title_id] <- -99
x <- as.factor(title_id.vec)
levels(x) <- 1:length(levels(x))
label <- as.numeric(x) - 1

##'
dtrain <- xgb.DMatrix(data = df.sample %>% select(-title_id) %>% as.matrix(),
                      label = label) 
number.of.classes <- label %>% unique() %>% length()
params <- list(booster = "gbtree", 
               objective = "multi:softprob", 
               eval_metric = "mlogloss",
               num_class = number.of.classes, 
               nthread = 15,
               eta=0.1, 
               gamma=0, 
               max_depth=4, 
               min_child_weight=1, 
               subsample=0.7, 
               colsample_bytree=0.7)
xgbcv <- xgb.cv(params = params, 
                data = dtrain, 
                nrounds = 3000, 
                nfold = 3, 
                showsd = T, 
                stratified = T, 
                # print_every_n = 10, 
                early_stopping_rounds = 20, 
                maximize = F, 
                verbose = 1, 
                prediction = TRUE)
bst <- xgboost(params = params, 
               data = dtrain, 
               nround = xgbcv$best_iteration, 
               verbose = 1)
## xgb.importance(colnames(df.sample %>% select(-title_id)), model = bst)
## importance_matrix <- xgb.importance(colnames(df.sample %>% select(-title_id)), model = bst)

##' make prediction
pred <- predict(bst, df.trsf.test %>% select(-title_id) %>% as.matrix())
pred.train <- predict(bst, df.trsf.train %>% select(-title_id) %>% as.matrix())
mapping.table <- data.frame(ori = title_id.vec, new = as.numeric(x)) %>% 
    group_by(ori, new) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    arrange(new) %>% 
    mutate(new = paste0("X", new))


submission_xgb_mclass <- sample
submission_xgb_mclass$title_id <- pred.df$ori

##'
test_prediction <- matrix(pred, nrow = number.of.classes,
                          ncol=length(pred)/number.of.classes) %>%
  t() %>%
  data.frame()
train_prediction <- matrix(pred.train, nrow = number.of.classes,
                          ncol=length(pred.train)/number.of.classes) %>%
  t() %>%
  data.frame()

colnames(test_prediction) <- mapping.table$ori
colnames(train_prediction) <- mapping.table$ori

##' select top 3 title_id and probability
test_pred_long <- data.frame(user_id = rep(user.test, each=ncol(test_prediction)), title_id = mapping.table$ori, pred = pred)
train_pred_long <- data.frame(user_id = rep(user.train, each=ncol(train_prediction)), title_id = mapping.table$ori, pred = pred.train)
max_prob <- test_pred_long %>% 
    group_by(user_id) %>% 
    filter(pred == max(pred))
second_prob <- test_pred_long %>% 
    arrange(user_id, -pred) %>% 
    group_by(user_id) %>% 
    filter(row_number() == 2)
third_prob <- test_pred_long %>% 
    arrange(user_id, -pred) %>% 
    group_by(user_id) %>% 
    filter(row_number() == 3)