library(xgboost)
library(Matrix)
library(caret)
library(reshape2)
user.train <- event_train$user_id %>% unique()
user.test <- event_test$user_id %>% unique()

##' feature making
user_f <- event_all %>% 
  group_by(user_id) %>% 
  summarise(cnt = n(), 
            min_month = month(min(date)),                           ##' factor
            max_month = month(max(date)),                           ##' factor
            date_diff = max_month - min_month,                      ##' numeric
            n_dist_title = n_distinct(title_id),                    ##' numeric
            last_title_time = watch_time[row_number() == n()],      ##' numeric
            last_title = title_id[row_number() == n()]              ##' factor
            ) %>% 
  mutate(min_month = as.factor(min_month), 
         max_month = as.factor(max_month),
         last_title = as.factor(last_title)
         )

user.title.cnt <- dcast.data.table(event_all, user_id ~ title_id, value.var = "watch_time", fun.aggregate = length)
user.title.watch_time.mean <- dcast.data.table(event_all, user_id ~ title_id, value.var = "watch_time", fun.aggregate = mean)
user.title.watch_time.sum <- dcast.data.table(event_all, user_id ~ title_id, value.var = "watch_time", fun.aggregate = sum)


##' one-hot-encoding
dmy <- dummyVars("~.", data = user_f, sparse = T)
df.all.trsf <- predict(dmy, newdata = user_f) %>% as.data.frame()
df.all.trsf <- df.all.trsf %>% 
  left_join(user.title.cnt, by = "user_id") %>% 
  left_join(user.title.watch_time.mean, by = "user_id") %>% 
  left_join(user.title.watch_time.sum, by = "user_id")

##' join answer the dataframe
df.all.x.y <- df.all.trsf %>% 
  left_join(label_train %>% mutate(title_id = as.factor(title_id)), by = "user_id")

##' split into training and testing
df.trsf.train <- df.all.x.y %>% filter(user_id %in% user.train) %>% select(-user_id)
df.trsf.test <- df.all.x.y %>% filter(user_id %in% user.test) %>% select(-user_id)
