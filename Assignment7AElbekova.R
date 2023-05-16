# Aidai Elbekova
# Assignment 7

library("tidytext")
library("ggplot2")
library("tidyverse")
library("igraph")
library("ggraph")

#1. Bring the book in tidy format, one-word-per-row.
mybook <- readRDS("../desktop/text_mining/datasets/mybooks3.rds") 
unique(mybook$title)
mybook_Machine <- mybook %>%
  filter(title=="The Time Machine") %>%
  mutate(chapter=cumsum(str_detect(text, 
                                   regex("^chapter",ignore_case = TRUE)))) %>%
  unnest_tokens(word, text)

  
#2. Check if the Zipf’s law holds for this book (by plotting the actual frequency of the words 
#and expected frequency under Zipf’s law in the same graph).
  wordfreq <- mybook_Machine %>%
    count(word, sort = TRUE) %>%
    mutate(word = factor(word, levels = word),
           rank = row_number(),
           zipfs_freq=ifelse(rank==1, n, dplyr::first(n)/rank))
  wordfreq2 <- gather(wordfreq, observ, freq, c(n, zipfs_freq)) 
  ggplot(wordfreq2, aes(x = rank, y = freq, color = observ)) + 
    geom_point(alpha=0.5, size=2) +
    theme_bw()+
    scale_colour_brewer(palette = "Set1")
  
#3. Clean the text from the stopwords.
  data(stop_words)
  tidybook <- mybook_Machine %>% anti_join(stop_words)
  
#4. Visualize the most common 20 words in the book by using a bar plot.
  tidybook %>%
    count(word, sort = TRUE) %>% 
    head(20) %>%  
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(n, word)) + 
    geom_col(fill="maroon", color="black") + 
    labs(y = NULL) + 
    theme_bw()
  
#5. Visualize the most common 15 bigrams in the book by using a bar plot.
  tale_bigrams <- mybook %>%
    filter(title=="The Time Machine") %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2)
  mostcommon_bi <- tale_bigrams %>% count(bigram, sort = TRUE) 
  bigrams_separated <- tale_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE) %>%
    filter(!is.na(word1)) 
  bigram_counts %>%  
    filter(!is.na(word1)) %>%
    mutate(word_pair=paste(word1, word2, sep=" ")) %>%  
    mutate(word_pair = reorder(word_pair, n)) %>% 
    head(15) %>%
    ggplot(aes(n, word_pair)) + 
    geom_col(fill="darkred", color="red") + 
    labs(y = NULL) + 
    theme_bw()
  
#6. Visualize the most common 10 trigrams in the book by using a bar plot.
  tale_trigrams <- mybook %>%
    filter(title=="The Time Machine") %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>%
    count(word1, word2, word3, sort = TRUE)
  tale_trigrams %>%  
    filter(!is.na(word1)) %>%
    mutate(words = paste(word1, word2, word3, sep = " ")) %>%  
    mutate(words = reorder(words, n)) %>% 
    head(10) %>%
    ggplot(aes(n, words)) + 
    geom_col(fill = "darkgreen", color = "green") + 
    labs(y = NULL) + 
    theme_bw()
  
#7. Create a network of common bigrams in the book.
  bigram_graph <- bigram_counts %>%
    filter(n > 3) %>%
    graph_from_data_frame()
  set.seed(54321)
  ggraph(bigram_graph, layout = "fr") + 
    geom_edge_link(color="blue") + 
    geom_node_point() + 
    theme_void() + geom_node_text(aes(label=name), vjust=1, hjust=1)