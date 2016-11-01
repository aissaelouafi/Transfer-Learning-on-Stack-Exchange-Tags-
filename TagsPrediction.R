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

#Data vizualisation 
#Visualize the most frequent tags/artciles category 
travel_tags <- str_split(travel$tags," ")
travel_tags_count <- unique(travel_tags)

#The number of distinct tags by category 
#In this case we consider tag as multiple word
travel_tags <- travel %>% group_by(tags) %>% summarise(n_distinct(tags))
biology_tags <- biology %>% group_by(tags) %>% summarise(n_distinct(tags))
robotic_tags <- robotic %>% group_by(tags) %>% summarise(n_distinct(tags))
cooking_tags <- cooking %>% group_by(tags) %>% summarise(n_distinct(tags))
crypto_tags <- crypto %>% group_by(tags) %>% summarise(n_distinct(tags))
diy_tags <- diy %>% group_by(tags) %>% summarise(n_distinct(tags))

nrow(travel_tags)
nrow(biology_tags)
nrow(robotic_tags)
nrow(cooking_tags)
nrow(crypto_tags)
nrow(diy_tags)

#Considering tag as single word : 
#Lets count the tags frequency by topic
travel_tags <- as.data.frame(as.factor(unlist(str_split(travel_tags$tags," "))))
colnames(travel_tags) <- c("tags","Freq")
travel_tags <- data.frame(table(travel_tags$tags))
#Sort data by tags frequency 
travel_tags <- travel_tags[order(travel_tags$Freq,decreasing = TRUE),]
travel_tags <- travel_tags[1:20,]

copy_to(sc,travel_tags)
#Data vizualisation
p <- plot_ly(x = travel_tags$tags, y = travel_tags$Freq,type="bar") %>%
  layout(yaxis = list(title = "Freq"),xaxis = list(title = "Tags"))

