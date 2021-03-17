## First approach using random forest

library(readr)
library(dplyr)
library(tm)
library(SnowballC)
library(randomForest)
library(caTools)
library(class)
library(tidytext)
library(ggplot2)

tag_extract <- function(tag) {
  tag %>% 
    gsub("<", "", .) %>% 
    gsub(">", " ", .) %>% 
    trimws() %>% 
    return()
}

all_same_char <- function(s) {
  return(length(unique(unlist(strsplit(s, "")))) == 1)
}

posts <- read_csv("post_data.csv") %>% 
  rename_all(tolower) %>% 
  rename(ori_rate="y") %>% 
  mutate(line = row_number(), rating = gsub("_.+", "", ori_rate), text = paste(title, body)) %>% 
  mutate(text = gsub("[^ -~]", "", text)) %>% 
  mutate(text = gsub("<[a-z]+>", "", text)) %>% 
  mutate(tags = tag_extract(tags)) %>% 
  mutate(text = paste(text, tags))

posts_tidy <- posts %>% 
  unnest_tokens(word, text) %>% 
  select(line, word, rating) %>% 
  anti_join(stop_words, by = "word") %>% 
  group_by(line, word, rating) %>% 
  summarise(n = n()) %>% 
  filter(grepl("[[:alpha:]|\\+\\+|#]", word)) %>% 
  filter(!grepl("[[:digit:]]+", word)) %>% 
  filter(!all_same_char(word)) %>% 
  filter(nchar(word) < 15)

posts_freqs <- posts_tidy %>% 
  group_by(rating, word) %>% 
  summarise(n_total = sum(n)) %>% 
  arrange(desc(n_total))

## add this plot later
posts_freqs %>% 
  top_n(15) %>% 
  arrange(n_total) %>%
  ggplot(aes(x = word, y = n_total, fill = rating)) + 
  geom_bar(stat = "identity") +
  ylab("Frequency") +
  coord_flip() +
  facet_wrap(~rating, scale = "free")

posts_dtm <- cast_dtm(posts_tidy, line, word, n)

posts_sparse <- removeSparseTerms(posts_dtm, 0.99)
posts_df <- as_tibble(as.matrix(posts_sparse))

posts_df <- as_tibble(as.matrix(posts_sparse)) %>%
  rename_all(~ {make.names(.x)})
posts_df$post_rating <- as.factor(posts$rating)

split <- sample.split(posts_df$post_rating, SplitRatio = 0.7)

posts_train <- posts_df[split, ]
posts_test <- posts_df[!split, ]

rf_model <- randomForest(post_rating  ~ ., data = posts_train)

preds <- predict(rf_model, posts_test)

table(posts_test$post_rating, preds)

sum(posts_test$post_rating == preds)/length(posts_test$post_rating)

# knnmod <- knn(train = posts_train[, colnames(posts_train) != "post_rating"], 
#               test = posts_test[, colnames(posts_test) != "post_rating"], 
#               cl = posts_train$post_rating, k = 1)
