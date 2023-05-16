#Aidai Elbekova
# Assignment 6

library("tm")
library(SnowballC)
library(wordcloud)

# 1. Download the recipe3.rds from moodle to your working directory and import the file to
# R studio (assign a name to your character vector).
recipevector2 <-  readLines("../desktop/text_mining/datasets/recipe2.txt")


# 2. Preprocess the text data; normalize the text, clean the text from all punctuation, numbers, 
# stopwords, weird characters, and stem the words.
mycorpus_recipe <- Corpus(VectorSource(recipevector2))
mycorpus_receipe_clean <- tm_map(mycorpus_recipe, content_transformer(tolower))
mycorpus_recipe_clean <- tm_map(mycorpus_receipe_clean, 
                         content_transformer(removePunctuation))
toSpace <- content_transformer(function(x, pattern){
  return (gsub(pattern, " ", x))})
mycorpus_recipe_clean <- tm_map(mycorpus_recipe_clean, toSpace, "—")
mycorpus_recipe_clean <- tm_map(mycorpus_recipe_clean, 
                         content_transformer(removeNumbers))
mycorpus_recipe_clean <- tm_map(mycorpus_recipe_clean, 
                         removeWords, stopwords("english"))
mycorpus_recipe_clean <- tm_map(mycorpus_recipe_clean, 
                         content_transformer(stripWhitespace))
mycorpus_recipe_clean <- tm_map(mycorpus_recipe_clean, stemDocument)

# 3. Make a bar plot of the words that show up more than 25 times. 
mydtm_recipe <- DocumentTermMatrix(mycorpus_recipe_clean)
inspect(mydtm_recipe)
dim(mydtm_recipe)

myfreq_recipe <- sort(colSums(as.matrix(mydtm_recipe)), decreasing=TRUE)
mydf_recipe <- data.frame(term = names(myfreq_recipe), occurrences = myfreq_recipe) 
mydf_recipe$term <- factor(mydf_recipe$term, levels=mydf_recipe$term[order(mydf_recipe$occurrences)])

mydf_recipe %>%   filter(myfreq_recipe > 25) %>% 
  ggplot(aes(term, occurrences)) + 
  geom_bar(stat = "identity", fill = "blue", color = "black") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

# 4. Create a wordcloud of words which show up at least 20 times.
wordcloud(names(myfreq_recipe), myfreq_recipe, max.stat.word=20)
