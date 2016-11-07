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
library(factoextra) # PCA 
library(slam)

#Get dev sparlyr version
devtools::install_github("rstudio/sparklyr")

#Remove all from memory
rm(list=ls())

#Sparklyr connection
sc <- spark_connect(master="local")

# Import Data
biology =  read.csv("../biology.csv")
travel = read.csv("../travel.csv",nrows=1000) # Get only first 1000 rows to construct the model for memory issues
travel_test = read.csv("../travel.csv",nrows = 1000 ,skip= 18279) #Predict on last 1000 rows of travel
colnames(travel_test) <- colnames(travel)
robotic = read.csv("../robotics.csv",nrows=1000)
cooking = read.csv("../cooking.csv",nrows = 1000)
crypto = read.csv("../crypto.csv",nrows = 1000)
diy = read.csv("../diy.csv",nrows = 1000)
test = read.csv("../test.csv",nrows = 1000)
sample_submission = read.csv("../sample_submission.csv")

#Copy data to spark environment 
#biology_sc <- as.data.frame(biology)
cc <- copy_to(sc,travel)

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


# Prepare the corpus from an input dataset dataframe
# input : df : the dataframe, column : df column to construct the Corpus
# output : tfidfx matrix 

generateCorpus <- function(df){
  corpus <- Corpus(VectorSource(df))
  replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
  corpus <- tm_map(corpus,replacePunctuation)
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,tolower)
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace) 
  corpus <- tm_map(corpus, PlainTextDocument)
  #options(mc.cores=1)
  #TrigramTokenizer <- function(x) NGramTokenizer(x, 
  #                                               Weka_control(min = 2, max = 2))
  #tdm <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
  return(corpus)
}

############################################################
#                                                          #
#                   DATA VIZUALISATION                     #  
#                                                          #
############################################################

#Count tags frequency by topic and vizualise the bar chart of most frequent tags
travel_tags_chart <- countDistinctTags(travel,10)
biology_tags_chart <- countDistinctTags(biology,50)
cooking_tags_chart <- countDistinctTags(cooking,10)

titleTagsProbability(cooking)

############################################################
#                                                          #
#                      NLP Analysis                        #  
#                                                          #
############################################################

# Topic titles corpus prepocessing
titles_corpus <- generateCorpus(biology$content)
title_corpus_test <- generateCorpus(test$content)
tfidf_test <- DocumentTermMatrix(title_corpus_test, control = list(weighting = weightTfIdf))

# Create the document-term matrix and the tf-idf matrix for both train and test data 
dtm <- DocumentTermMatrix(titles_corpus)
tfidf <- DocumentTermMatrix(titles_corpus, control = list(weighting = weightTfIdf))
freq <- sort(colSums(as.matrix(dtm)),decreasing = TRUE)

# Plot the most frequent word in topic titles
wf <- data.frame(word=names(freq), freq=freq)[1:30,]
p <- plot_ly(x = wf$word, y = wf$freq,type="bar") %>%
  layout(yaxis = list(title = "Freq"),xaxis = list(title = "Topic title"))

#Word cloud of topic titles & Word cloud of topic tags
wordcloud(names(freq),freq,min.freq = 10)

#Convert the tfidf matrix to dataframe
tfidf <- as.data.frame(as.matrix(tfidf))

#Analyze a small part from the tfidf matrix for test
rownames(tfidf) <- 1:nrow(tfidf)

#ANNEX : Copy the tfidf matrix into spark context
tfidf_sc <- copy_to(sc,tfidf)



#Dimensionality reduction : PCA Analysis based on the small tfidf dataframe
tfidf.pca <- prcomp(tfidf)
names(tfidf.pca)
head(tfidf.pca$sdev)

#Study variances of the principal components
#Eigenvalues (valeur propre): Eigenvalues measures the variability retained by each PC. It's large for the first PC and small for the subsequent PCs.
eig <- (tfidf.pca$sdev)^2
#Variance
variance <- eig*100/sum(eig)
#Cumulative variance
cumvariance <- cumsum(variance) # The total of the cumulative variance should be egal to 100

#Let's construct a dataframe with all this informations 
eig.dataframe <- data.frame(eig = eig, variance = variance, cumvariance = cumvariance)

#See the importance of principal composents (PCs) using a scree plot
eig.dataframe <- eig.dataframe[1:50,]
barplot(eig.dataframe[,2],
        names.arg = 1:nrow(eig.dataframe),
        main="Variance scree plot",
        xlab = "Principal Component",
        ylab = "Percentage of the variance",
        col = "steelblue")

lines(x=1:nrow(eig.dataframe),eig.dataframe[,2],type="b",pch=10, col="red")

#Another alternative to plot with using the factoextra library
fviz_screeplot(tfidf.pca, ncp=10)

# Note that a good dimension reduction is achieved when the first few PCs account for a large porportion of the variability (80-90%)
fviz_pca_var(tfidf.pca)

#Graph of variables (The correlation circle)
#In order to extract the results for variables contributions in princal components of the PCA
#using get_pca_var we get a list of matrices containing all results for active variables (cordinates, correlation between variables and axes, squared cosinus and contributions)
var_contr <- get_pca_var(tfidf.pca)

#let show the variables contributions to construct the 10 first PCs of our 
var_contr_dim <- as.data.frame(var_contr$contrib)[,1:10]

#let show the cordinates of variables in the first 4 PCs
var_cordinates <- as.data.frame(var_contr$coord)[1:10,1:4]

#let plot the correlation circle of variables with the PCs
a <- seq(0,2*pi, length = 100)
plot(cos(a),sin(a),type = 'l', color = 'gray',xlab = "PC1", ylab = "PC2")
abline(h = 0, v = 0, lty = 2)
arrows(0,0,var_cordinates[,1], var_cordinates[,2], length = 0.5, angle = 15, code = 2)
text(var_cordinates, labels=rownames(var_cordinates), cex = 1, adj=1)


fviz_pca_var(tfidf.pca)

#Build the naive bayes classifier 
model <- naiveBayes(as.matrix(tfidf),as.factor(biology$tags))
tfidf_test <- rollup(tfidf_test, 2, na.rm=TRUE, FUN = sum)
prediction <- predict(model,as.matrix(tfidf_test))
colnames(prediction) <- c("tags")
prediction <- as.data.frame(prediction)


#Show the most frequent predicted tags 
frequent_predicted_tags <- as.data.frame(table(prediction))
frequent_predicted_tags <- frequent_predicted_tags[order(frequent_predicted_tags$Freq,decreasing = TRUE),]
frequent_predicted_tags <- frequent_predicted_tags[1:10,]
p <- plot_ly(x = frequent_predicted_tags$prediction, y = frequent_predicted_tags$Freq,type="bar") %>%
  layout(yaxis = list(title = "Freq"),xaxis = list(title = "Tags"))

#Evaluate the model 