#Aidai Elbekova
# Assignment 8
library("topicmodels")
library("tidyverse")
library("dplyr")
library("tidytext")
library("ggplot2")
library("tm")
library("e1071")
library("gmodels")

# 1. Build a Naive Bayes classifier that learns the difference between the lines of The Picture of 
# Dorian Gray and The Time Machine novels. You can find the books in moodle in mybooks3.rds file. 
# Use the half of the lines, to model your classifier and allocate 85% of the text for training dataset. 
# Evaluate the performance of your classifier and report the accuracy of your model.

mybooks <-  readRDS("../desktop/text_mining/datasets/mybooks3.rds")
dim(mybooks)
colnames(mybooks)

# Select a sample
samplelines <- sample(seq_len(nrow(mybooks)), size = 5793)
mysample <- mybooks[samplelines,]

# Split the training and test datasets
picked <- sample(seq_len(nrow(mysample)),size = 4924)
Data_train <- mysample[picked,]
Data_test <- mysample[-picked,]

# Preprocessing
Data_train_corpus <-Corpus(VectorSource(Data_train$text))
Data_test_corpus <-Corpus(VectorSource(Data_test$text))

Data_train_corpus_clean <- tm_map(Data_train_corpus, 
                                  content_transformer(tolower))
Data_test_corpus_clean <- tm_map(Data_test_corpus, 
                                 content_transformer(tolower))

Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, 
                                  content_transformer(removeNumbers))
Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, 
                                 content_transformer(removeNumbers))

Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, 
                                  content_transformer(removePunctuation))
Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, 
                                 content_transformer(removePunctuation))

Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, 
                                  removeWords,stopwords("english"))
Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, 
                                 removeWords, stopwords("english"))

Data_train_corpus_clean <- tm_map(Data_train_corpus_clean, 
                                  content_transformer(stripWhitespace))
Data_test_corpus_clean <- tm_map(Data_test_corpus_clean, 
                                 content_transformer(stripWhitespace))
# Transform into document term matrix
Data_train_dtm <- DocumentTermMatrix(Data_train_corpus_clean)
Data_test_dtm <- DocumentTermMatrix(Data_test_corpus_clean)

# Changing the weights of the DTM to binary
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1)) 
  return(x)}

Data_train_dtm <- apply(Data_train_dtm, MARGIN = 2, convert_counts)
Data_test_dtm <- apply(Data_test_dtm, MARGIN = 2, convert_counts)
class(Data_test_dtm)

#Train NB classiffier
myclassifier <- naiveBayes(Data_train_dtm, Data_train$title)

#Evaluate models
data_test_pred <- predict(myclassifier, Data_test_dtm)
CrossTable(data_test_pred, Data_test$title,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c("predicted", "actual"))

# Model accuracy
# (484+153)/ 869 = 73,3%

# 2. For this question, use only the lines from The Picture of Dorian Gray book. 
dorian_book <- mybooks %>%
  group_by(title) %>%
  filter(title=="The Picture of Dorian Gray")%>%
  ungroup()
  

#Preprocessing the text data
tidy_dorian <- dorian_book %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)
  
# Fit an LDA model to model the topics in each chapter in the book. 
dorian_dtm <- tidy_dorian %>%
  cast_dtm(word, word, n)

topic_model <- LDA(dorian_dtm, k = 8, 
                   control = list(seed = 1234))
word_topics <- tidy(topic_model, matrix = "beta")

# Choose a number of topics between 4-8 and visualize the word-topic probabilities for each topic. 
word_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL) + 
  theme_minimal()

mydocuments <- tidy(topic_model, matrix = "gamma")
mydocuments %>% head(5)

# Experiment with the number of topics and other parameters of the LDA function so that 
# the word-topic probabilities make the most sense. 
#4 topics
topic_model_4 <- LDA(dorian_dtm, k = 4, 
                   control = list(seed = 1234))
word_topics_4 <- tidy(topic_model_4, matrix = "beta")

word_topics_4 %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL) + 
  theme_minimal()

mydocuments_4 <- tidy(topic_model_4, matrix = "gamma")
mydocuments_4 %>% head(5)

#6 topics
topic_model_6 <- LDA(dorian_dtm, k = 6, 
                     control = list(seed = 1234))
word_topics_6 <- tidy(topic_model_6, matrix = "beta")

word_topics_6 %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL) + 
  theme_minimal()

mydocuments_6 <- tidy(topic_model_6, matrix = "gamma")
mydocuments_6 %>% head(5)
# Report the results of the model you think makes sense the most. 
# After you select the optimum number of topics, visualize the topic distribution of each chapter.
word_topics_4 <- tidy(topic_model,
                    matrix = "gamma",
                    document_names = rownames(lyrics_sparse))

word_topics_4 %>%
  mutate(chapter_topics = fct_reorder(document, gamma),
         topic = factor(topic)) %>%
  ggplot(aes(gamma, topic, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(chapter_topics), ncol = 4) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = expression(gamma), y = "Topic")
