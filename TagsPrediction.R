#Import library
library(sparklyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(plotly)

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
  return(df)
}




############################################################
#                                                          #
#                   DATA VIZUALISATION                     #  
#                                                          #
############################################################

#Count tags frequency by topic
travel_tags <- countDistinctTags(travel,50)
biology_tags <- countDistinctTags(biology,50)

#Data vizualisation
p <- plot_ly(x = biology_tags$tags, y = biology_tags$Freq,type="bar") %>%
  layout(yaxis = list(title = "Freq"),xaxis = list(title = "Tags"))
p

#Lets analyze the probability that a tag is in title
title_words <- sapply(str_split(travel$title," "),'[',1:max(lengths(str_split(travel$title," "))))
title_words <- t(title_words[,1:ncol(title_words)])
tag_words <- sapply(str_split(travel$tags," "),'[',1:max(lengths(str_split(travel$tags," "))))
tag_words <- t(tag_words[,1:ncol(tag_words)])

tag_in_title <- tag_words %in% title_words
table(tag_in_title)["TRUE"] / length(tag_in_title)