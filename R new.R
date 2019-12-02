install.packages("sentimentr")
install.packages("tm")
install.packages("tidyverse")
install.packages("stringr")
install.packages("dplyr")
install.packages("tidytext")
install.packages("textdata")
install.packages("wordcloud")
install.packages("igraph")
install.packages("ggraph")


library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(sentimentr)
library(dplyr)
library(tm)
library(textstem)
library(tidyverse)    
library(stringr)  
library("readxl")
library(tidytext)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)
#Import and prepare the variables to use ahead
DM <-  read_csv("script.csv") 
DMD <- read_csv("script.csv")

DMT <- tibble(DM)
DM


#Start with Text Mining (Perform tokenization,Frequency Count, Sentiment Analysis)
DF <- data.frame(DM$TEXT,stringsAsFactors = FALSE)








xy<-  tibble(word=DM$TEXT)
## Tokenization of text (converting into one word per row and storing in another tibble)

xy1<-xy %>% unnest_tokens(word,word)

## Removing the stop words from the tibble by using anti-join with stop_words data set present in tidyverse
xy1 <- xy1 %>% 
  anti_join(stop_words)

## Performing basic Sentiment Analysis using NRC
xy1 %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

## Performing basic Sentiment Analysis using BING
xy1 %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

## Getting thr word cloud of positive and negative terms
xy1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 50)


#Implementing social Network map
Char<- read.csv("script1.csv")

Char1 <- Char %>% select('character.A', 'character.B')


length(unique(Char1$`character.A`))
unique(Char1$`character.A`)

length(unique(Char1$`character.B`))
unique(Char1$`character.B`)
conversations <- Char1 %>% group_by(`character.A`,`character.B`) %>% summarise(counts = n())

# taking the most talked charecters
conversations<-conversations[order(conversations$counts,decreasing = TRUE),]



set.seed(42) # setting a seed allows us to select the same sample every time

# Taking unique Nodes 
nodes <- c(as.character(conversations$character.A), as.character(conversations$character.B))
nodes <- unique(nodes)

# Plotting the graph
g <- graph_from_data_frame(conversations, directed = TRUE)

plot(g, vertex.size=4,
      vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
 

 
 ##Plotting most used words in the script
 cor <- iconv(DM$TEXT)
 cor <- Corpus(VectorSource(cor))
 inspect(cor[1:5])
 
 cor<- tm_map(cor,tolower)
 cor<- tm_map(cor,removePunctuation)
 cor<- tm_map(cor,removeNumbers)
 cor<- tm_map(cor,removeWords,stopwords('English'))
 cor<- tm_map(cor,removeWords,c('deathlyhallowspart','harrypotterforever','abcfamily','harrypotterweekend','potterheadweekend','starts','marathon','harrypotter','watching'
                                ,'potter','dont','harry'))
 cor<- tm_map(cor,stripWhitespace)
 tdm <- TermDocumentMatrix(cor)
 tdm <- as.matrix(tdm)
 w<-rowSums(tdm)
 w<- subset(w, w>=25)
 w
 barplot(w,las=2, col=rainbow(50))
 
 #Plotting the sentiment of the text

 c <- iconv(DM$TEXT)
 
 cor<- tm_map(cor,tolower)
 cor<- tm_map(cor,removePunctuation)
 cor<- tm_map(cor,removeNumbers)
 cor<- tm_map(cor,removeWords,stopwords('English'))
 cor<- tm_map(cor,removeWords,c('deathlyhallowspart','harrypotterforever','abcfamily','harrypotterweekend','potterheadweekend','starts','marathon','harrypotter','watching'
                                ,'potter','dont','harry'))
 cor<- tm_map(cor,stripWhitespace)
 tdm <- TermDocumentMatrix(cor)
 tdm <- as.matrix(tdm)
 
 s <- get_nrc_sentiment(c)
 barplot(colSums(s),las=2 ,col="Green4")
 
 
 
 
