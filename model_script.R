## First approach using random forest

library(readr)
library(dplyr)
library(tm)
library(SnowballC)
library(randomForest)
library(caTools)
library(class)

posts <- read_csv("post_data.csv") %>% 
  rename_all(tolower) %>% 
  rename(ori_rate="y") %>% 
  mutate(rating = gsub("_.+", "", ori_rate))

posts_corpus <- Corpus(VectorSource(posts$body)) %>% 
  tm_map(PlainTextDocument) %>% 
  tm_map(tolower) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('en')) %>% 
  tm_map(stemDocument)

posts_dtm <- DocumentTermMatrix(posts_corpus)

posts_sparse <- removeSparseTerms(posts_dtm, 0.97)

posts_df <- as_tibble(as.matrix(posts_sparse)) %>%
  rename_all(~ {make.names(.x)}) %>% 
  mutate(post_rating = as.factor(posts$rating))

split <- sample.split(posts_df$post_rating, SplitRatio = 0.7)

posts_train <- posts_df[split, ]
posts_test <- posts_df[!split, ]

# rf_model <- randomForest(post_rating  ~ ., data = posts_train)
# 
# preds <- predict(rf_model, posts_test)
# 
# table(posts_test$post_rating, preds)

library("doMC")

registerDoMC(4)
darkAndScaryForest <- foreach(y=seq(10), .combine=combine ) %dopar% {
  rf <- randomForest(post_rating  ~ ., data = posts_train, ntree=50, norm.votes=FALSE)
}


# knnmod <- knn(train = posts_train[, colnames(posts_train) != "post_rating"], 
#               test = posts_test[, colnames(posts_test) != "post_rating"], 
#               cl = posts_train$post_rating, k = 1)