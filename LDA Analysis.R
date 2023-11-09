
setwd("C:/Users/admin/Downloads")
dt=read.csv("Issues.csv",stringsAsFactors=FALSE,header=T)
head(dt,5)
myvars <- c("title", "abstract")
newdata <- dt[myvars]
head(newdata,5)
write.csv(newdata, '/Users/admin/Downloads/new_file.csv')
###Title analysis
myvars <- c("title")
text <- dt[myvars]
str(text,5)
set.seed(318846870)
head(text, n =5)

text[,sapply(text,is.character)] <- sapply(
  text[,sapply(text,is.character)],
  iconv,"WINDOWS-1252","UTF-8")

frequency_dataframe = text %>% count(title) %>% arrange(desc(n))
print(frequency_dataframe,10)
#####Data Preparation
library(tm)
text_corpus <- VCorpus(VectorSource(text$title))
print(text_corpus)
text_corpus_clean <- tm_map(text_corpus,
                            content_transformer(tolower))
text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)
text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
text_corpus_clean <- tm_map(text_corpus_clean,
                            removeWords, stopwords())
text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

#####Data exploration
library(wordcloud)
wordcloud(text_corpus_clean, min.freq = 10, random.order = FALSE,
          colors=brewer.pal(8, "Dark2"))

#######Model Training
text_dtm <- DocumentTermMatrix(text_corpus_clean)
text_dtm
findFreqTerms(text_dtm, highfreq = 20) ## Words that occur at least 20 times in the entire dataset
findFreqTerms(text_dtm, lowfreq = 20) ## Words that occur at least 20 times in the entire dataset

######2LDA Topic models
library(topicmodels)
text_lda <- LDA(text_dtm, k = 2, method = "VEM", control = NULL)
text_lda
######Model Evaluation
library(ggplot2)
library(dplyr)
text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

######Abstract Analysis
myvars <- c("abstract")
text <- dt[myvars]
str(text,5)
set.seed(318846870)
head(text, n =5)

text[,sapply(text,is.character)] <- sapply(
  text[,sapply(text,is.character)],
  iconv,"WINDOWS-1252","UTF-8")


#####Data Preparation
library(tm)
text_corpus <- VCorpus(VectorSource(text$title))
print(text_corpus)
text_corpus_clean <- tm_map(text_corpus,
                            content_transformer(tolower))
text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)
text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
text_corpus_clean <- tm_map(text_corpus_clean,
                            removeWords, stopwords())
text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

#####Data exploration
library(wordcloud)
wordcloud(text_corpus_clean, min.freq = 10, random.order = FALSE,
          colors=brewer.pal(8, "Dark2"))
#######Model Training
text_dtm <- DocumentTermMatrix(text_corpus_clean)
text_dtm
findFreqTerms(text_dtm, lowfreq = 20) ## Words that occur at least 20 times in the entire dataset

######2LDA Topic models
library(topicmodels)
text_lda <- LDA(text_dtm, k = 2, method = "VEM", control = NULL)
text_lda
######Model Evaluation
library(ggplot2)
library(dplyr)
text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
##############################End#############################
