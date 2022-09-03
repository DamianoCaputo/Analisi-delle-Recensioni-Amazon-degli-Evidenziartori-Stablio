#IMPORTO IL DATASET E LO CHIAMO x----
library(readr)
EVIDENZIATORI <- read_csv("R/EVIDENZIATORI.csv")
#LIBRERIE-----
library(textclean)
library(ggplot2)
library(tm)
library(ggthemes)
library(wordcloud2)
library(sentimentr)
library(tidyverse)
library(topicmodels)
library(tidytext)
#CONTROLLO DEI DATI E ANALISI SU DI ESSI-----
x <- EVIDENZIATORI
View(x)

t <- c("Reviewed in the United Kingdom on")
l <- c(".0 out of 5")
x$DATE<- mgsub(pattern=t, replacement="",x$DATE)
x$RATING<- mgsub(pattern=l, replacement="", x$RATING)

x$DATE <- gsub(" January ", "/01/", x$DATE)
x$DATE <- gsub(" February ", "/02/",x$DATE)
x$DATE <- gsub(" March ", "/03/", x$DATE)
x$DATE <- gsub(" April ", "/04/", x$DATE)
x$DATE <- gsub(" May ", "/05/", x$DATE)
x$DATE <- gsub(" June ", "/06/", x$DATE)
x$DATE <- gsub(" July ", "/07/", x$DATE)
x$DATE <- gsub(" August ", "/08/", x$DATE)
x$DATE <- gsub(" September ", "/09/", x$DATE)
x$DATE <- gsub(" October ", "/10/", x$DATE)
x$DATE <- gsub(" November ", "/11/",x$DATE)
x$DATE <- gsub(" December ", "/12/", x$DATE)
x$DATE <- as.Date(x$DATE,format = "%d/%m/%Y")

x$DATE <- format(x$DATE, format="%Y")
#CONTROLLO FREQUENZA DELLE STELLE e DELLE DATE----
stars <- table(x$RATING)
stars

date <- table(x$DATE)
date
#RENDO LE TABELLE UN DATAFRAME-----
s <- data.frame(stars)
colnames(s)<-c("Rating","Frequency")#rinomino le colonne 

d <- data.frame(date)
colnames(d)<-c("Date","Frequency")
#GRAFICO FREQUENZA DELLE STELLE-----
#frequenza complessiva 
p <- ggplot(data=s, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.5) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY')
p
#frequenza delle stelle annuale
date_1 <- subset(x, x["DATE"] == "2013")
f_date <- table(date_1$RATING)
f_date <- data.frame(f_date)
colnames(f_date)<-c("Rating","Frequency")
g_2013 <- ggplot(data=f_date, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.25) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2013')
g_2013

date_2 <- subset(x, x["DATE"] == "2015")
f_date_2 <- table(date_2$RATING)
f_date_2 <- data.frame(f_date_2)
colnames(f_date_2)<-c("Rating","Frequency")
g_2015 <- ggplot(data=f_date_2, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.25) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2015')
g_2015

date_3 <- subset(x, x["DATE"] == "2016")
f_date_3 <- table(date_3$RATING)
f_date_3 <- data.frame(f_date_3)
colnames(f_date_3)<-c("Rating","Frequency")
g_2016 <- ggplot(data=f_date_3, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.25) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2016')
g_2016

date_4 <- subset(x, x["DATE"] == "2017")
f_date_4 <- table(date_4$RATING)
f_date_4 <- data.frame(f_date_4)
colnames(f_date_4)<-c("Rating","Frequency")
g_2017 <- ggplot(data=f_date_4, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.25) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2017')
g_2017

date_5 <- subset(x, x["DATE"] == "2018")
f_date_5 <- table(date_5$RATING)
f_date_5 <- data.frame(f_date_5)
colnames(f_date_5)<-c("Rating","Frequency")
g_2018 <- ggplot(data=f_date_5, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.5) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2018')
g_2018

date_6 <- subset(x, x["DATE"] == "2019")
f_date_6 <- table(date_6$RATING)
f_date_6 <- data.frame(f_date_6)
colnames(f_date_6)<-c("Rating","Frequency")
g_2019 <- ggplot(data=f_date_6, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.5) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2019')
g_2019

date_7 <- subset(x, x["DATE"] == "2020")
f_date_7 <- table(date_7$RATING)
f_date_7 <- data.frame(f_date_7)
colnames(f_date_7)<-c("Rating","Frequency")
g_2020 <- ggplot(data=f_date_7, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.5) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2020')
g_2020

date_8 <- subset(x, x["DATE"] == "2021")
f_date_8 <- table(date_8$RATING)
f_date_8 <- data.frame(f_date_8)
colnames(f_date_8)<-c("Rating","Frequency")
g_2021 <- ggplot(data=f_date_8, aes(x=Frequency, y= Rating)) +
  geom_bar(stat='identity',width=0.5) +
  geom_text(aes(label=Frequency), colour="white", hjust=1.25,size=5.0)+
  ggtitle('STARS FREQUENCY 2021')
g_2021

#PULIZIA(totale) E FREQUENZA DELLE PAROLE-----
##recensioni----
text.df <- EVIDENZIATORI
review <- data.frame(ID=seq(1:nrow(text.df)), text=text.df$TEXT)
tryTolower <- function(x){
  y = NA
  try_error= tryCatch(tolower(x), error=function(e) e)#provare a togliere il trycatch
  if (!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus,content_transformer(tryTolower))
  corpus <- tm_map(corpus,removeWords,stopwords("en"))
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,removeNumbers)
  return(corpus)
}
corpus <- VCorpus(VectorSource(review))
corpus <- clean.corpus(corpus)
corpus_pulito <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
corpus_pulito$text.1 <- NULL 
colnames(corpus_pulito)<- c("Text")
tdm <- TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.review <- as.matrix(tdm)
freq <- rowSums(tdm.review)
freq.df <- data.frame(word=names(freq), frequency=freq) 
freq.df <- freq.df[order(freq.df[,2], decreasing = T),]
freq.df$word <- factor(freq.df$word,levels=unique(as.character(freq.df$word)))
##titoli----
title <- data.frame(ID=seq(1:nrow(text.df)), text=text.df$TITLE)
corpus_2 <- VCorpus(VectorSource(title))
corpus_2 <- clean.corpus(corpus_2)
corpus_2_pulito <- data.frame(text = sapply(corpus_2, as.character), stringsAsFactors = FALSE)
corpus_2_pulito$text.1 <- NULL 
colnames(corpus_2_pulito)<- c("Text")
tdm_2 <- TermDocumentMatrix(corpus_2, control=list(weighting=weightTf))
tdm.title <- as.matrix(tdm_2)
freq_2 <- rowSums(tdm.title)
freq.df_2 <- data.frame(word=names(freq_2), frequency=freq_2)
freq.df_2 <- freq.df_2[order(freq.df_2[,2], decreasing = T),]
freq.df_2$word <- factor(freq.df_2$word,levels=unique(as.character(freq.df_2$word)))
#GRAFICO DELLE PAROLE DELLE RECENSIONI:----
##non saranno mostrate tutte ma solo quelle con più frequenza
graph <- ggplot(freq.df[1:50,], aes(x=word,y=frequency))+
  geom_bar(stat="identity",width=0.9)+
  coord_flip()+
  theme_gdocs()+
  geom_text(aes(label=frequency), colour="white", hjust=1.25,size=3.0)+
  ggtitle('WORDS FREQUENCY')
graph
#GRAFICO DELLE PAROLE DEI TITOLI:----
##non saranno mostrate tutte ma solo quelle con più frequenza
graph_2 <- ggplot(freq.df_2[1:50,], aes(x=word,y=frequency))+
  geom_bar(stat="identity",width=0.9)+
  coord_flip()+
  theme_gdocs()+
  geom_text(aes(label=frequency), colour="white", hjust=1.25,size=3.0)+
  ggtitle('FREQUENCY OF TITLE WORDS')
graph_2
#WORD CLOUD
wordcloud2(data=freq.df, size = 0.7, shape = 'circle')
#SENTIMENT ANALYSIS
##sentiment delle frasi singole
recensioni <- x$TEXT
recensioni <- as.data.frame(recensioni)
colnames(recensioni)<-c("Text")
sentiment_review <- recensioni %>%
  get_sentences()%>%
  sentiment()%>%
  mutate(polarity_level = ifelse(sentiment < 0.2, "Negative",
                                 ifelse(sentiment > 0.2, "Positive","Neutral")))

grafico_a <- sentiment_review%>%
  ggplot()+geom_density(aes(sentiment))+ggtitle("SENTIMENT DENSITY")
grafico_a

grafico_b <- sentiment_review%>%
  ggplot()+geom_bar(aes(x=polarity_level),width=0.25)+ggtitle("POLARITY LEVEL")
grafico_b

##sentiment recensioni complete
sentiment_2 <- sentiment_by(recensioni$Text[1:500])

sen_graph <- sentiment_2%>%
  ggplot()+
  geom_histogram(aes(x=sentiment_2$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram")
sen_graph
##sentiment dei titoli 
titoli <- x$TITLE
titoli <- as.data.frame(titoli)
colnames(titoli) <- c("Title")

sentiment_title <- sentiment_by(titoli$Title[1:500])

title_graph <- sentiment_title%>%
  ggplot()+
  geom_histogram(aes(x=sentiment_title$ave_sentiment),binwidth=0.1)+
  ggtitle("Title Sentiment Histogram")
title_graph
##sentiment delle recensioni per anno 
Date <- x$DATE
Text <- x$TEXT
z_1 <- data.frame(Date,Text)

sent_1 <- subset(z_1, z_1["Date"] == "2013")
s_1 <- sentiment_by(sent_1$Text[1:4])
sen_graph_2013 <- s_1%>%
  ggplot()+
  geom_histogram(aes(x=s_1$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2013")
sen_graph_2013

sent_2 <- subset(z_1, z_1["Date"] == "2015")
s_2 <- sentiment_by(sent_2$Text[1:7])
sen_graph_2015 <- s_2%>%
  ggplot()+
  geom_histogram(aes(x=s_2$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2015")
sen_graph_2015

sent_3 <- subset(z_1, z_1["Date"] == "2016")
s_3 <- sentiment_by(sent_3$Text[1:4])
sen_graph_2016 <- s_3%>%
  ggplot()+
  geom_histogram(aes(x=s_3$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2016")
sen_graph_2016

sent_4 <- subset(z_1, z_1["Date"] == "2017")
s_4 <- sentiment_by(sent_4$Text[1:32])
sen_graph_2017 <- s_4%>%
  ggplot()+
  geom_histogram(aes(x=s_4$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2017")
sen_graph_2017

sent_5 <- subset(z_1, z_1["Date"] == "2018")
s_5 <- sentiment_by(sent_5$Text[1:63])
sen_graph_2018 <- s_5%>%
  ggplot()+
  geom_histogram(aes(x=s_5$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2018")
sen_graph_2018

sent_6 <- subset(z_1, z_1["Date"] == "2019")
s_6 <- sentiment_by(sent_6$Text[1:137])
sen_graph_2019 <- s_6%>%
  ggplot()+
  geom_histogram(aes(x=s_6$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2019")
sen_graph_2019

sent_7 <- subset(z_1, z_1["Date"] == "2020")
s_7 <- sentiment_by(sent_7$Text[1:165])
sen_graph_2020 <- s_7%>%
  ggplot()+
  geom_histogram(aes(x=s_7$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2020")
sen_graph_2020

sent_8 <- subset(z_1, z_1["Date"] == "2020")
s_8 <- sentiment_by(sent_8$Text[1:88])
sen_graph_2021 <- s_8%>%
  ggplot()+
  geom_histogram(aes(x=s_8$ave_sentiment),binwidth=0.1)+
  ggtitle("Review Sentiment Histogram 2021")
sen_graph_2021
#TOPIC MODELLING
testi <- x$TEXT
testi <- as.data.frame(testi)

reviews_source <- VCorpus(VectorSource(testi))
reviews_source <- clean.corpus(reviews_source) 
dtm <- DocumentTermMatrix(reviews_source)

lda <- LDA(dtm, k = 4, control = list(seed = 1234))
topics <- tidy(lda, matrix = "beta") 

topic_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

topic_graph <- topic_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
topic_graph

