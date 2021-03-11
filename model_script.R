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
  mutate(line = row_number(), rating = gsub("_.+", "", ori_rate), body = paste(title, body))

posts_tidy <- posts %>% 
  mutate(body = gsub("[^ -~]", "", body)) %>% 
  mutate(body = gsub("<[a-z]+>", "", body)) %>% 
  mutate(tags = tag_extract(tags)) %>% 
  mutate(body = paste(body, tags)) %>% 
  unnest_tokens(word, body) %>% 
  select(line, word, rating) %>% 
  anti_join(stop_words, by = "word") %>% 
  group_by(line, word, rating) %>% 
  summarise(n = n()) %>% 
  filter(grepl("[[:alpha:]|\\+\\+|#]", word)) %>% 
  filter(!grepl("[[:digit:]]", word)) %>% 
  filter(!all_same_char(word)) %>% 
  filter(nchar(word) < 15)
  
posts_dtm <- cast_dtm(posts_tidy, line, word, n)

#print(inspect(posts_dtm))

# posts_tidy <- posts %>% 
#   unnest_tokens(word, body) %>% 
#   select(word, rating) %>% 
#   group_by(word, rating) %>% 
#   summarise(n = n()) %>% 
#   filter(!grepl("[[:punct:]|[:digit:]]", word), nchar(word) > 2) %>% 
#   filter(n > 5) %>% 
#   anti_join(stop_words, by = "word")

# test1 <- posts %>% 
#   mutate(line = row_number()) %>% 
#   unnest_tokens(word, body) %>% 
#   select(line, word, rating) %>% 
#   group_by(line, word, rating) %>% 
#   summarise(n = n()) %>% 
#   filter(!grepl("[[:punct:]|[:digit:]]", word), nchar(word) > 2) %>% 
#   filter(n > 5) %>% 
#   anti_join(stop_words, by = "word")


#add_stop_words <- posts_tidy$word[c(1:48, (nrow(posts_tidy) - 2):nrow(posts_tidy))]

#posts_tidy <- posts_tidy[49:(nrow(posts_tidy) - 2), ]

# posts_corpus <- Corpus(VectorSource(posts$body)) %>%
#   tm_map(PlainTextDocument) %>%
#   tm_map(tolower) %>%
#   tm_map(removePunctuation) %>%
#   tm_map(removeNumbers) %>%
#   tm_map(removeWords, c(stopwords('en'), add_stop_words)) %>%
#   tm_map(stemDocument)

# posts_dtm <- DocumentTermMatrix(posts_corpus)
# posts_dtm2 <- posts_tidy %>% 
#   ungroup() %>% 
#   mutate(line = row_number()) %>% 
#   cast_dtm(line, word, n)

posts_sparse <- removeSparseTerms(posts_dtm, 0.997)
posts_df <- as_tibble(as.matrix(posts_sparse))

posts_df <- as_tibble(as.matrix(posts_sparse)) %>%
  rename_all(~ {make.names(.x)})
posts_df$post_rating <- posts$rating

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



  
posts_tidy %>%
  group_by(rating) %>%
  top_n(10, word) %>%
  ungroup() %>% 
  arrange(rating, -word)


  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()