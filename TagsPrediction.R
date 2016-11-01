#Import library
library(sparklyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(plotly)

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
biology_sc <- as.data.frame(biology)
scc <- copy_to(sc,biology_sc)

############################################################
#                                                          #
#                   FUNCTIONS                              #  
#                                                          #
############################################################






############################################################
#                                                          #
#                   DATA VIZUALISATION                     #  
#                                                          #
############################################################

#Count tags frequency by topic
travel_tags <- as.data.frame(as.factor(unlist(str_split(travel$tags," "))))
colnames(travel_tags) <- c("tags")
travel_tags <- data.frame(table(travel_tags$tags))
colnames(travel_tags) <- c("tags","Freq")

#Sort data by tags frequency 
travel_tags <- travel_tags[order(travel_tags$Freq,decreasing = TRUE),]

#Get the 20 most frequent tags 
travel_tags <- travel_tags[1:20,]

#Data vizualisation
p <- plot_ly(x = travel_tags$tags, y = travel_tags$Freq,type="bar") %>%
  layout(yaxis = list(title = "Freq"),xaxis = list(title = "Tags"))

