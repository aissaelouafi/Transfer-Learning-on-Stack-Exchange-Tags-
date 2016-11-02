#Import library
library(sparklyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(plotly)
library(tm)
library(wordcloud)
library(e1071) #Naive bayes classifier 
library(stats)

#Remove all from memory
rm(list=ls())

#Sparklyr connection
sc <- spark_connect(master="local")

# Import Data
biology =  read.csv("../biology.csv")
travel = read.csv("../travel.csv")
robotic = read.csv("../robotics.csv")
cooking = read.csv("../cooking.csv")
crypto = read.csv("../crypto.csv")
diy = read.csv("../diy.csv")
test = read.csv("../test.csv")
sample_submission = read.csv("../sample_submission.csv")

#Copy data to spark environment 
#biology_sc <- as.data.frame(biology)
#scc <- copy_to(sc,biology_sc)

############################################################
#                                                          #
#                   FUNCTIONS                              #  
#                                                          #
############################################################


# Count most frequent tags by topic 
# input : dataframe
# nFreq : minimal frequency of tags

countDistinctTags <- function(df,nFreq){
  df <- as.data.frame(as.factor(unlist(str_split(df$tags," "))))
  colnames(df) <- c("tags")
  df <- data.frame(table(df$tags))
  colnames(df) <- c("tags","Freq")
  df <- df[order(df$Freq,decreasing = TRUE),]
  df <- df[1:nFreq,]
  p <- plot_ly(x = df$tags, y = df$Freq,type="bar") %>%
    layout(yaxis = list(title = "Freq"),xaxis = list(title = "Tags"))
  return(p)
}

# Analyze the appareance probability of tags in the topic title
# input : df the data frame to analyze

titleTagsProbability <- function(df){
  title_words <- sapply(str_split(df$title," "),'[',1:max(lengths(str_split(df$title," "))))
  title_words <- t(title_words[,1:ncol(title_words)])
  tag_words <- sapply(str_split(df$tags," "),'[',1:max(lengths(str_split(df$tags," "))))
  tag_words <- t(tag_words[,1:ncol(tag_words)])
  
  tag_in_title <- tag_words %in% title_words
  return(table(tag_in_title)["TRUE"] / length(tag_in_title))
}

############################################################
#                                                          #
#                   DATA VIZUALISATION                     #  
#                                                          #
############################################################

#Count tags frequency by topic and vizualise the bar chart of most frequent tags
travel_tags_chart <- countDistinctTags(travel,50)
biology_tags_chart <- countDistinctTags(biology,50)
cooking_tags_chart <- countDistinctTags(cooking,10)

titleTagsProbability(cooking)

############################################################
#                                                          #
#                      NLP Analysis                        #  
#                                                          #
############################################################

# Topic titles corpus prepocessing
titles_corpus <- Corpus(VectorSource(travel$title))
replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
titles_corpus <- tm_map(titles_corpus,replacePunctuation)
titles_corpus <- tm_map(titles_corpus,removePunctuation)
titles_corpus <- tm_map(titles_corpus,removeNumbers)
titles_corpus <- tm_map(titles_corpus,tolower)
titles_corpus <- tm_map(titles_corpus,removeWords,stopwords("english"))
titles_corpus <- tm_map(titles_corpus, stripWhitespace) 
titles_corpus <- tm_map(titles_corpus, PlainTextDocument) 

# Create the document-term matrix and the tf-idf matrix
dtm <- DocumentTermMatrix(titles_corpus)
tfidf <- DocumentTermMatrix(titles_corpus, control = list(weighting = weightTfIdf))
freq <- sort(colSums(as.matrix(dtm)),decreasing = TRUE)

# Plot the most frequent word in topic titles
wf <- data.frame(word=names(freq), freq=freq)[1:30,]
p <- plot_ly(x = wf$word, y = wf$freq,type="bar") %>%
  layout(yaxis = list(title = "Freq"),xaxis = list(title = "Topic title"))

#Word cloud of topic titles & Word cloud of topic tags
wordcloud(names(freq),freq,min.freq = 250)

#Convert the tfidf matrix to dataframe
tfidf <- as.data.frame(as.matrix(tfidf))

#Analyze a small part from the tfidf matrix for test
tfidf_small <- tfidf[1:100,]
rownames(tfidf_small) <- 1:nrow(tfidf_small)

#Dimensionality reduction : PCA Analysis 
log.tfidf <- log(tfidf)

#Build the naive bayes classifier 
model <- naiveBayes(as.matrix(tfidf),as.factor(travel$tags))
