
## Data Science Capstone
library(NLP)
library(tm)
library(qdapRegex)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(wordcloud)
library(sqldf)
library(akmeans)
library(NbClust)
library(lsa)
library(ape)
library(arules)
library(fpc)
library(gridExtra)
library(dendextend)
##library(gridExtra)

## Mac
setwd("/Users/jpmoraga/Desktop/Data Science Capstone")

## Windows
## setwd("C:/Users/jmoraga/Desktop/respaldo jpmoraga/Data Science Certification/Data Science Capstone")

source("Functions.R")

## Mac
setwd("/Users/jpmoraga/Desktop/Data Science Capstone/Data")

## Windows
## setwd("C:/Users/jmoraga/Desktop/respaldo jpmoraga/Data Science Certification/Data Science Capstone/Data")

# filepath <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# 
# download.file(filepath,paste(getwd(),"Coursera-SwiftKey.zip",sep = "/"))
# 
# unzip("Coursera-SwiftKey.zip")

##setwd("de_DE")

############################################# Quiz 1 ###################################################
setwd(paste(getwd(),"Final/en_US",sep = "/"))

rbind(file.info(list.files()[1]),file.info(list.files()[2]),file.info(list.files()[3]))

con1 <- file(list.files()[1], "r")
con2 <- file(list.files()[2], "r")
con3 <- file(list.files()[3], "r")

en_Blogs <- readLines(con1, encoding = "UTF-8")
en_News <- readLines(con2, encoding = "UTF-8")
en_Tweets <- readLines(con3, encoding = "UTF-8")

close(con1)
close(con2)
close(con3)

df <- data.frame(
list(Blogs = c(Size_bytes = object.size(en_Blogs),Lines = length(en_Blogs)),
     News = c(Size_bytes = object.size(en_News),Lines = length(en_News)),
     Tweets = c(Size_bytes = object.size(en_Tweets),Lines = length(en_Tweets)))
)


## Files size chart
bs <- barplot(c(Blogs=df$Blogs[1],News=df$News[1],Tweets=df$Tweets[1]), xlab = "Size (bytes)", main = "Files size", axes = FALSE)
text(x = bs, y = c(df$Blogs[1],df$News[1],df$Tweets[1]), labels = c(df$Blogs[1],df$News[1],df$Tweets[1]), pos = 1)

## Files lines chart
bl <- barplot(c(df$Blogs[2],df$News[2],df$Tweets[2]), xlab = "Lines", main = "Lines", axes = FALSE, ylim = c(0,3000000))
text(x = bl, y = c(df$Blogs[2],df$News[2],df$Tweets[2]), labels = c(df$Blogs[2],df$News[2],df$Tweets[2]), pos = 3)


head(sort(nchar(en_Blogs), decreasing = TRUE),1)
head(sort(nchar(en_News), decreasing = TRUE),1)
head(sort(nchar(en_Tweets), decreasing = TRUE),1)

sample_size <- 30000

set.seed(1)
sample_Blogs <- sample(en_Blogs, sample_size) 
sample_News <- sample(en_News, sample_size)
sample_Tweets <- sample(en_Tweets, sample_size)

# # Create clean-up functions
# cleanText = function(x){
#   #   This simple function does not cover ambiguities such as 's or 'd
#   x <- gsub("let's","let us",x)
#   x <- gsub("I'm","I am",x)
#   x <- gsub("'re", " are",x)
#   x <- gsub("n't", " not",x)
#   x <- gsub("'ll", " will",x)
#   x <- gsub("'ve"," have",x)
#   x <- gsub("â€™|â€œ|â€", "", x)
#   x <- gsub("[^a-zA-Z ]", "", x)
#   return(x)
# }

## Clean text
sample_Blogs <- sapply(sample_Blogs, cleanText)
sample_News <- sapply(sample_News, cleanText)
sample_Tweets <- sapply(sample_Tweets, cleanText)

## Remove emoticons
sample_Blogs <- rm_emoticon(sample_Blogs, trim = TRUE ,clean = TRUE)
sample_News <- rm_emoticon(sample_News, trim = TRUE ,clean = TRUE)
sample_Tweets <- rm_emoticon(sample_Tweets, trim = TRUE ,clean = TRUE)

## Remove lines with length = 0
sample_Blogs <- sample_Blogs[nchar(sample_Blogs) != 0]
sample_News <- sample_News[nchar(sample_News) != 0]
sample_Tweets <- sample_Tweets[nchar(sample_Tweets) != 0]

## tolower
sample_Blogs <- tolower(sample_Blogs)
sample_News <- tolower(sample_News)
sample_Tweets <- tolower(sample_Tweets)

word_Blogs <- sapply(sample_Blogs, wordpunct_tokenizer)
word_News <- sapply(sample_News, wordpunct_tokenizer)
word_Tweets <- sapply(sample_Tweets, wordpunct_tokenizer)

########################## Analysis including stop words #############################

## Tokenization

Token_Blogs <- NULL

    for(i in 1:length(sample_Blogs))
    {
      s <- sample_Blogs[i]
      w <- word_Blogs[[i]]
      s <- String(s)
      s <- s[w]
      Token_Blogs[[length(Token_Blogs)+1]] <- s
      i = i + 1
    }

Token_News <- NULL

    for(i in 1:length(sample_News))
    {
      s <- sample_News[i]
      w <- word_News[[i]]
      s <- String(s)
      s <- s[w]
      Token_News[[length(Token_News)+1]] <- s
      i = i + 1
    }

Token_Tweets <- NULL

    for(i in 1:length(sample_Tweets))
    {
      s <- sample_Tweets[i]
      w <- word_Tweets[[i]]
      s <- String(s)
      s <- s[w]
      Token_Tweets[[length(Token_Tweets)+1]] <- s
      i = i + 1
    }

############### Frequencies analysis including stopwords #################

## 1-gram 

## Blogs
All_Blogs <- as.data.frame(unlist(Token_Blogs))
colnames(All_Blogs) <- "Words"

Freq_Blogs <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Blogs GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Blogs_bar <- Freq_Blogs[head(order(-Freq_Blogs$Words), 20),]


ggplot(Freq_Blogs_bar, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart (Blogs)") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Blogs$W, Freq_Blogs$Words, min.freq = 2000, max.words = 50, random.order=FALSE)

## News
All_News <- as.data.frame(unlist(Token_News))
colnames(All_News) <- "Words"

Freq_News <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_News GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_News_bar <- Freq_News[head(order(-Freq_News$Words), 20),]


ggplot(Freq_News_bar, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_News$W, Freq_News$Words, min.freq = 1000, max.words = 50, random.order=FALSE)


## Tweets
All_Tweets <- as.data.frame(unlist(Token_Tweets))
colnames(All_Tweets) <- "Words"

Freq_Tweets <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Tweets GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Tweets_bar <- Freq_Tweets[head(order(-Freq_Tweets$Words), 20),]


ggplot(Freq_Tweets_bar, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Tweets$W, Freq_Tweets$Words, min.freq = 50, max.words = 50, random.order=FALSE)

## All together
All_Words <- rbind(All_Blogs, All_News, All_Tweets)
colnames(All_Words) <- "Words"

Freq_Words <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Words GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Words_bar <- Freq_Words[head(order(-Freq_Words$Words), 20),]


ggplot(Freq_Words_bar, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Words$W, Freq_Words$Words, min.freq = 2000, max.words = 50, random.order=FALSE)

## 2-gram

Token_Blogs2 <- Token_Blogs[lengths(Token_Blogs, use.names = TRUE) > 1]
Token_News2 <- Token_News[lengths(Token_News, use.names = TRUE) > 1]
Token_Tweets2 <- Token_Tweets[lengths(Token_Tweets, use.names = TRUE) > 1]

## Blogs 2

ng2_Blogs <- NULL
for (j in 1:length(Token_Blogs2))
      {
      ng2 <- ngrams(unlist(Token_Blogs2[[j]]), 2L)
      n2 <- NULL
      for (k in 1:length(ng2))
                {
                  n1 <- NULL
                  n <- unlist(ng2[k])
                  n1 <- paste(n[1],n[2],sep = " ")
                  n2 <- rbind(n2,n1)
                  k = k + 1
                  }
      ng2_Blogs <- rbind(ng2_Blogs,n2)
      print(c(j,length(Token_Blogs2),j*100/length(Token_Blogs2), "2-blogs"))
      }

All_Blogs2 <- as.data.frame(ng2_Blogs, row.names = seq(1:nrow(ng2_Blogs)))
colnames(All_Blogs2) <- "Words"
remove(ng2_Blogs)

Freq_Blogs2 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Blogs2 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Blogs_bar2 <- Freq_Blogs2[head(order(-Freq_Blogs2$Words), 20),]


ggplot(Freq_Blogs_bar2, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Blogs2$W, Freq_Blogs2$Words, min.freq = 50, max.words = 30, random.order=FALSE)

## News 2

ng2_News <- NULL
for (j in 1:length(Token_News2))
{
  ng2 <- ngrams(unlist(Token_News2[[j]]), 2L)
  n2 <- NULL
  for (k in 1:length(ng2))
  {
    n1 <- NULL
    n <- unlist(ng2[k])
    n1 <- paste(n[1],n[2],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng2_News <- rbind(ng2_News,n2)
  print(c(j,length(Token_News2),j*100/length(Token_News2), "2-news"))
}

All_News2 <- as.data.frame(ng2_News, row.names = seq(1:nrow(ng2_News)))
colnames(All_News2) <- "Words"
remove(ng2_News)

Freq_News2 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_News2 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_News_bar2 <- Freq_News2[head(order(-Freq_News2$Words), 20),]


ggplot(Freq_News_bar2, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_News2$W, Freq_News2$Words, min.freq = 50, max.words = 30, random.order=FALSE)


## Tweets 2

ng2_Tweets <- NULL
for (j in 1:length(Token_Tweets2))
{
  ng2 <- ngrams(unlist(Token_Tweets2[[j]]), 2L)
  n2 <- NULL
  for (k in 1:length(ng2))
  {
    n1 <- NULL
    n <- unlist(ng2[k])
    n1 <- paste(n[1],n[2],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng2_Tweets <- rbind(ng2_Tweets,n2)
  print(c(j,length(Token_Tweets2),j*100/length(Token_Tweets2), "2-tweets"))
}

All_Tweets2 <- as.data.frame(ng2_Tweets, row.names = seq(1:nrow(ng2_Tweets)))
colnames(All_Tweets2) <- "Words"
remove(ng2_Tweets)

Freq_Tweets2 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Tweets2 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Tweets_bar2 <- Freq_Tweets2[head(order(-Freq_Tweets2$Words), 20),]


ggplot(Freq_Tweets_bar2, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Tweets2$W, Freq_Tweets2$Words, min.freq = 50, max.words = 30, random.order=FALSE)

## All together 2
All_Words2 <- rbind(All_Blogs2, All_News2, All_Tweets2)
colnames(All_Words2) <- "Words"

Freq_Words2 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Words2 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Words_bar2 <- Freq_Words2[head(order(-Freq_Words2$Words), 20),]


ggplot(Freq_Words_bar2, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Words2$W, Freq_Words2$Words, min.freq = 50, max.words = 50, random.order=FALSE)

## 3-gram

Token_Blogs3 <- Token_Blogs[lengths(Token_Blogs, use.names = TRUE) > 2]
Token_News3 <- Token_News[lengths(Token_News, use.names = TRUE) > 2]
Token_Tweets3 <- Token_Tweets[lengths(Token_Tweets, use.names = TRUE) > 2]

## Blogs 3

ng3_Blogs <- NULL
for (j in 1:length(Token_Blogs3))
{
  ng3 <- ngrams(unlist(Token_Blogs3[[j]]), 3L)
  n2 <- NULL
  for (k in 1:length(ng3))
  {
    n1 <- NULL
    n <- unlist(ng3[k])
    n1 <- paste(n[1],n[2],n[3],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng3_Blogs <- rbind(ng3_Blogs,n2)
  print(c(j,length(Token_Blogs3),j*100/length(Token_Blogs3), "3-blogs"))
}

All_Blogs3 <- as.data.frame(ng3_Blogs, row.names = seq(1:nrow(ng3_Blogs)))
colnames(All_Blogs3) <- "Words"
remove(ng3_Blogs)

Freq_Blogs3 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Blogs3 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Blogs_bar3 <- Freq_Blogs3[head(order(-Freq_Blogs3$Words), 20),]


ggplot(Freq_Blogs_bar3, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Blogs3$W, Freq_Blogs3$Words, min.freq = 50, max.words = 20, random.order=FALSE)

## News 3

ng3_News <- NULL
for (j in 1:length(Token_News3))
{
  ng3 <- ngrams(Token_News3[[j]], 3L)
  n2 <- NULL
  for (k in 1:length(ng3))
  {
    n1 <- NULL
    n <- unlist(ng3[k])
    n1 <- paste(n[1],n[2],n[3],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng3_News <- rbind(ng3_News,n2)
  print(c(j,length(Token_News3),j*100/length(Token_News3), "3-news"))
}

All_News3 <- as.data.frame(ng3_News, row.names = seq(1:nrow(ng3_News)))
colnames(All_News3) <- "Words"
remove(ng3_News)

Freq_News3 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_News3 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_News_bar3 <- Freq_News3[head(order(-Freq_News3$Words), 20),]


ggplot(Freq_News_bar3, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_News3$W, Freq_News3$Words, min.freq = 50, max.words = 20, random.order=FALSE)


## Tweets 3

ng3_Tweets <- NULL
for (j in 1:length(Token_Tweets3))
{
  ng3 <- ngrams(unlist(Token_Tweets3[[j]]), 3L)
  n2 <- NULL
  for (k in 1:length(ng3))
  {
    n1 <- NULL
    n <- unlist(ng3[k])
    n1 <- paste(n[1],n[2],n[3],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng3_Tweets <- rbind(ng3_Tweets,n2)
  print(c(j,length(Token_Tweets3),j*100/length(Token_Tweets3), "3-tweets"))
}

All_Tweets3 <- as.data.frame(ng3_Tweets, row.names = seq(1:nrow(ng3_Tweets)))
colnames(All_Tweets3) <- "Words"
remove(ng3_Tweets)

Freq_Tweets3 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Tweets3 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Tweets_bar3 <- Freq_Tweets3[head(order(-Freq_Tweets3$Words), 20),]


ggplot(Freq_Tweets_bar3, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Tweets3$W, Freq_Tweets3$Words, min.freq = 50, max.words = 20, random.order=FALSE)

## All together 3
All_Words3 <- rbind(All_Blogs3, All_News3, All_Tweets3)
##colnames(All_Words3) <- "Words"

Freq_Words3 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Words3 GROUP BY Words ORDER BY COUNT(Words) DESC")
Freq_Words_bar3 <- Freq_Words3[head(order(-Freq_Words3$Words), 20),]


ggplot(Freq_Words_bar3, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Words3$W, Freq_Words3$Words, min.freq = 50, max.words = 20, random.order=FALSE)

## 4-gram

Token_Blogs4 <- Token_Blogs[lengths(Token_Blogs, use.names = TRUE) > 3]
Token_News4 <- Token_News[lengths(Token_News, use.names = TRUE) > 3]
Token_Tweets4 <- Token_Tweets[lengths(Token_Tweets, use.names = TRUE) > 3]

## Blogs 4

ng4_Blogs <- NULL
for (j in 1:length(Token_Blogs4))
{
  ng4 <- ngrams(unlist(Token_Blogs4[[j]]), 4L)
  n2 <- NULL
  for (k in 1:length(ng4))
  {
    n1 <- NULL
    n <- unlist(ng4[k])
    n1 <- paste(n[1],n[2],n[3],n[4],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng4_Blogs <- rbind(ng4_Blogs,n2)
  print(c(j,length(Token_Blogs4),j*100/length(Token_Blogs4), "4-blogs"))
}

All_Blogs4 <- as.data.frame(ng4_Blogs, row.names = seq(1:nrow(ng4_Blogs)))
colnames(All_Blogs4) <- "Words"
remove(ng4_Blogs)

Freq_Blogs4 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Blogs4 GROUP BY Words ORDER BY COUNT(Words) DESC")


## News 4

ng4_News <- NULL
for (j in 1:length(Token_News4))
{
  ng4 <- ngrams(Token_News4[[j]], 4L)
  n2 <- NULL
  for (k in 1:length(ng4))
  {
    n1 <- NULL
    n <- unlist(ng4[k])
    n1 <- paste(n[1],n[2],n[3],n[4],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng4_News <- rbind(ng4_News,n2)
  print(c(j,length(Token_News4),j*100/length(Token_News4), "4-news"))
}

All_News4 <- as.data.frame(ng4_News, row.names = seq(1:nrow(ng4_News)))
colnames(All_News4) <- "Words"
remove(ng4_News)

Freq_News4 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_News4 GROUP BY Words ORDER BY COUNT(Words) DESC")


## Tweets 4

ng4_Tweets <- NULL
for (j in 1:length(Token_Tweets4))
{
  ng4 <- ngrams(unlist(Token_Tweets4[[j]]), 4L)
  n2 <- NULL
  for (k in 1:length(ng4))
  {
    n1 <- NULL
    n <- unlist(ng4[k])
    n1 <- paste(n[1],n[2],n[3],n[4],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng4_Tweets <- rbind(ng4_Tweets,n2)
  print(c(j,length(Token_Tweets4),j*100/length(Token_Tweets4), "4-tweets"))
}

All_Tweets4 <- as.data.frame(ng4_Tweets, row.names = seq(1:nrow(ng4_Tweets)))
colnames(All_Tweets4) <- "Words"
remove(ng4_Tweets)

Freq_Tweets4 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Tweets4 GROUP BY Words ORDER BY COUNT(Words) DESC")


## All together 4
All_Words4 <- rbind(All_Blogs4, All_News4, All_Tweets4)
##colnames(All_Words3) <- "Words"

Freq_Words4 <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Words4 GROUP BY Words ORDER BY COUNT(Words) DESC")


############### Frequencies analysis without stopwords #################

## Remove stopwords

## Create function
## nsw = function(x){x[x %in% stopwords() == FALSE]}

Token_Blogs_nsw <- lapply(Token_Blogs2,nsw)
Token_News_nsw <- lapply(Token_News2,nsw)
Token_Tweets_nsw <- lapply(Token_Tweets2,nsw)

## 1-gram 

## Blogs
All_Blogs_nsw <- as.data.frame(unlist(Token_Blogs_nsw))
colnames(All_Blogs_nsw) <- "Words"

Freq_Blogs_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Blogs_nsw GROUP BY Words")
Freq_Blogs_bar_nsw <- Freq_Blogs_nsw[head(order(-Freq_Blogs_nsw$Words), 20),]


ggplot(Freq_Blogs_bar_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart without stopword") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Blogs_nsw$W, Freq_Blogs_nsw$Words, min.freq = 50, max.words = 100, random.order=FALSE)

## News
All_News_nsw <- as.data.frame(unlist(Token_News_nsw))
colnames(All_News_nsw) <- "Words"

Freq_News_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_News_nsw GROUP BY Words")
Freq_News_bar_nsw <- Freq_News_nsw[head(order(-Freq_News_nsw$Words), 20),]


ggplot(Freq_News_bar_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_News_nsw$W, Freq_News_nsw$Words, min.freq = 50, max.words = 100, random.order=FALSE)


## Tweets
All_Tweets_nsw <- as.data.frame(unlist(Token_Tweets_nsw))
colnames(All_Tweets_nsw) <- "Words"

Freq_Tweets_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Tweets_nsw GROUP BY Words")
Freq_Tweets_bar_nsw <- Freq_Tweets_nsw[head(order(-Freq_Tweets_nsw$Words), 20),]


ggplot(Freq_Tweets_bar_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Tweets_nsw$W, Freq_Tweets_nsw$Words, min.freq = 50, max.words = 100, random.order=FALSE)

## All together
All_Words_nsw <- rbind(All_Blogs_nsw, All_News_nsw, All_Tweets_nsw)
colnames(All_Words_nsw) <- "Words"

Freq_Words_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Words_nsw GROUP BY Words")
Freq_Words_bar_nsw <- Freq_Words_nsw[head(order(-Freq_Words_nsw$Words), 20),]


ggplot(Freq_Words_bar_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle("1-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Words_nsw$W, Freq_Words_nsw$Words, min.freq = 50, max.words = 100, random.order=FALSE)

## 2-gram

Token_Blogs_nsw2 <- Token_Blogs_nsw[lengths(Token_Blogs_nsw, use.names = TRUE) > 1]
Token_News_nsw2 <- Token_News_nsw[lengths(Token_News_nsw, use.names = TRUE) > 1]
Token_Tweets_nsw2 <- Token_Tweets_nsw[lengths(Token_Tweets_nsw, use.names = TRUE) > 1]

## Blogs 2

ng2_Blogs_nsw <- NULL
for (j in 1:length(Token_Blogs_nsw2))
##for (j in 1:3)
{
  ng2 <- ngrams(unlist(Token_Blogs_nsw2[[j]]), 2L)
  n2 <- NULL
  for (k in 1:length(ng2))
  {
    n1 <- NULL
    n <- unlist(ng2[k])
    n1 <- paste(n[1],n[2],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng2_Blogs_nsw <- rbind(ng2_Blogs_nsw,n2)
  print(c(j,length(Token_Blogs_nsw2),j*100/length(Token_Blogs_nsw2), "2-blogs-nsw"))
}

All_Blogs2_nsw <- as.data.frame(ng2_Blogs_nsw)
colnames(All_Blogs2_nsw) <- "Words"
remove(ng2_Blogs_nsw)

Freq_Blogs2_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Blogs2_nsw GROUP BY Words")
Freq_Blogs_bar2_nsw <- Freq_Blogs2_nsw[head(order(-Freq_Blogs2_nsw$Words), 20),]


ggplot(Freq_Blogs_bar2_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Blogs2_nsw$W, Freq_Blogs2_nsw$Words, min.freq = 50, max.words = 50, random.order=FALSE)

## News 2

ng2_News_nsw <- NULL
for (j in 1:length(Token_News_nsw2))
{
  ng2 <- ngrams(unlist(Token_News_nsw2[[j]]), 2L)
  n2 <- NULL
  for (k in 1:length(ng2))
  {
    n1 <- NULL
    n <- unlist(ng2[k])
    n1 <- paste(n[1],n[2],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng2_News_nsw <- rbind(ng2_News_nsw,n2)
  print(c(j,length(Token_News_nsw2),j*100/length(Token_News_nsw2), "2-news-nsw"))
}

All_News2_nsw <- as.data.frame(ng2_News_nsw)
colnames(All_News2_nsw) <- "Words"
remove(ng2_News_nsw)

Freq_News2_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_News2_nsw GROUP BY Words")
Freq_News_bar2_nsw <- Freq_News2_nsw[head(order(-Freq_News2_nsw$Words), 20),]


ggplot(Freq_News_bar2_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_News2_nsw$W, Freq_News2_nsw$Words, min.freq = 50, max.words = 50, random.order=FALSE)


## Tweets 2

ng2_Tweets_nsw <- NULL
for (j in 1:length(Token_Tweets_nsw2))
{
  ng2 <- ngrams(unlist(Token_Tweets_nsw2[[j]]), 2L)
  n2 <- NULL
  for (k in 1:length(ng2))
  {
    n1 <- NULL
    n <- unlist(ng2[k])
    n1 <- paste(n[1],n[2],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng2_Tweets_nsw <- rbind(ng2_Tweets_nsw,n2)
  print(c(j,length(Token_Tweets_nsw2),j*100/length(Token_Tweets_nsw2), "2-tweets-nsw"))
}

All_Tweets2_nsw <- as.data.frame(ng2_Tweets_nsw)
colnames(All_Tweets2_nsw) <- "Words"
remove(ng2_Tweets_nsw)

Freq_Tweets2_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Tweets2_nsw GROUP BY Words")
Freq_Tweets_bar2_nsw <- Freq_Tweets2_nsw[head(order(-Freq_Tweets2_nsw$Words), 20),]


ggplot(Freq_Tweets_bar2_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Tweets2_nsw$W, Freq_Tweets2_nsw$Words, min.freq = 50, max.words = 50, random.order=FALSE)

## All together 2
All_Words2_nsw <- rbind(All_Blogs2_nsw, All_News2_nsw, All_Tweets2_nsw)
colnames(All_Words2_nsw) <- "Words"

Freq_Words2_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Words2_nsw GROUP BY Words")
Freq_Words_bar2_nsw <- Freq_Words2_nsw[head(order(-Freq_Words2_nsw$Words), 20),]


ggplot(Freq_Words_bar2_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words2") + ylab("Freq") + ggtitle("2-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Words2_nsw$W, Freq_Words2_nsw$Words, min.freq = 50, max.words = 50, random.order=FALSE)

## 3-gram

Token_Blogs_nsw3 <- Token_Blogs_nsw[lengths(Token_Blogs_nsw, use.names = TRUE) > 2]
Token_News_nsw3 <- Token_News_nsw[lengths(Token_News_nsw, use.names = TRUE) > 2]
Token_Tweets_nsw3 <- Token_Tweets_nsw[lengths(Token_Tweets_nsw, use.names = TRUE) > 2]

## Blogs 3

ng3_Blogs_nsw <- NULL
for (j in 1:length(Token_Blogs_nsw3))
{
  ng3 <- ngrams(Token_Blogs_nsw3[[j]], 3L)
  n2 <- NULL
  for (k in 1:length(ng3))
  {
    n1 <- NULL
    n <- unlist(ng3[k])
    n1 <- paste(n[1],n[2],n[3],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng3_Blogs_nsw <- rbind(ng3_Blogs_nsw,n2)
  print(c(j,length(Token_Blogs_nsw3),j*100/length(Token_Blogs_nsw3), "3-blogs-nsw"))
}

All_Blogs3_nsw <- as.data.frame(ng3_Blogs_nsw)
colnames(All_Blogs3_nsw) <- "Words"
remove(ng3_Blogs_nsw)

Freq_Blogs3_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Blogs3_nsw GROUP BY Words")
Freq_Blogs_bar3_nsw <- Freq_Blogs3_nsw[head(order(-Freq_Blogs3_nsw$Words), 20),]


ggplot(Freq_Blogs_bar3_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Blogs3_nsw$W, Freq_Blogs3_nsw$Words, min.freq = 50, max.words = 20, random.order=FALSE)

## News 3

ng3_News_nsw <- NULL
for (j in 1:length(Token_News_nsw3))
{
  ng3 <- ngrams(Token_News_nsw3[[j]], 3L)
  n2 <- NULL
  for (k in 1:length(ng3))
  {
    n1 <- NULL
    n <- unlist(ng3[k])
    n1 <- paste(n[1],n[2],n[3],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng3_News_nsw <- rbind(ng3_News_nsw,n2)
  print(c(j,length(Token_News_nsw3),j*100/length(Token_News_nsw3), "3-news-nsw"))
}

All_News3_nsw <- as.data.frame(ng3_News_nsw)
colnames(All_News3_nsw) <- "Words"
remove(ng3_News_nsw)

Freq_News3_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_News3_nsw GROUP BY Words")
Freq_News_bar3_nsw <- Freq_News3_nsw[head(order(-Freq_News3_nsw$Words), 20),]


ggplot(Freq_News_bar3_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_News3_nsw$W, Freq_News3_nsw$Words, min.freq = 50, max.words = 20, random.order=FALSE)


## Tweets 3

ng3_Tweets_nsw <- NULL
for (j in 1:length(Token_Tweets_nsw3))
{
  ng3 <- ngrams(Token_Tweets_nsw3[[j]], 3L)
  n2 <- NULL
  for (k in 1:length(ng3))
  {
    n1 <- NULL
    n <- unlist(ng3[k])
    n1 <- paste(n[1],n[2],n[3],sep = " ")
    n2 <- rbind(n2,n1)
    k = k + 1
  }
  ng3_Tweets_nsw <- rbind(ng3_Tweets_nsw,n2)
  print(c(j,length(Token_Tweets_nsw3),j*100/length(Token_Tweets_nsw3), "3-tweets-nsw"))
}

All_Tweets3_nsw <- as.data.frame(ng3_Tweets_nsw)
colnames(All_Tweets3_nsw) <- "Words"
remove(ng3_Tweets_nsw)

Freq_Tweets3_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Tweets3_nsw GROUP BY Words")
Freq_Tweets_bar3_nsw <- Freq_Tweets3_nsw[head(order(-Freq_Tweets3_nsw$Words), 20),]


ggplot(Freq_Tweets_bar3_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Tweets3_nsw$W, Freq_Tweets3_nsw$Words, min.freq = 50, max.words = 20, random.order=FALSE)

## All together 3
All_Words3_nsw <- rbind(All_Blogs3_nsw, All_News3_nsw, All_Tweets3_nsw)
colnames(All_Words3_nsw) <- "Words"

Freq_Words3_nsw <- sqldf("SELECT Words W, COUNT(Words) Words FROM All_Words3_nsw GROUP BY Words")
Freq_Words_bar3_nsw <- Freq_Words3_nsw[head(order(-Freq_Words3_nsw$Words), 20),]


ggplot(Freq_Words_bar3_nsw, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words3") + ylab("Freq") + ggtitle("3-gram Frequency chart without stopwords") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
wordcloud(Freq_Words3_nsw$W, Freq_Words3_nsw$Words, min.freq = 50, max.words = 20, random.order=FALSE)

########################## Cluster Analysis #############################

## Without stopwords

# Token_Blogs_nsw1 <- as.vector(Token_Blogs_nsw)
# Token_News_nsw1 <- as.vector(Token_News_nsw)
# Token_Tweets_nsw1 <- as.vector(Token_Tweets_nsw)

Corp_Blogs <- VCorpus(VectorSource(Token_Blogs_nsw))
Corp_News <- VCorpus(VectorSource(Token_News_nsw))
Corp_Tweets <- VCorpus(VectorSource(Token_Tweets_nsw))

Corp_Blogs <- TermDocumentMatrix(Corp_Blogs)
Corp_News <- TermDocumentMatrix(Corp_News)
Corp_Tweets <- TermDocumentMatrix(Corp_Tweets)

sparsity <- 0.98

Corp_Blogs <- removeSparseTerms(Corp_Blogs, sparsity)
Corp_News <- removeSparseTerms(Corp_News, sparsity)
Corp_Tweets <- removeSparseTerms(Corp_Tweets, sparsity)

Corp_Blogs <- as.matrix(Corp_Blogs)
Corp_News <- as.matrix(Corp_News)
Corp_Tweets <- as.matrix(Corp_Tweets)

Corp_Blogs <- Corp_Blogs[,colSums(Corp_Blogs) != 0]
Corp_News <- Corp_News[,colSums(Corp_News) != 0]
Corp_Tweets <- Corp_Tweets[,colSums(Corp_Tweets) != 0]

dim(Corp_Blogs)

dcos <- cosine(Corp_Blogs)

dcos <- as.dist(1-dcos)

hc <- hclust(dcos, "ward.D2")


## http://artax.karlin.mff.cuni.cz/r-help/library/NbClust/html/NbClust.html
res <- NbClust(t(Corp_Blogs), dcos, distance = NULL, max.nc = 100, method = "ward.D", index = "kl")
res$Best.nc

## https://rpubs.com/gaston/dendrograms
hc_Blogs <- cutree(hc, k = res$Best.nc[1])

hc_Blogs_df <- as.data.frame(hc_Blogs)

## Cluster size
table(hc_Blogs_df)

## Cluster Dendrogram colour

dend <- as.dendrogram(hc)
d2=color_branches(dend,k=res$Best.nc[1]) # auto-coloring 5 clusters of branches.
plot(d2)


## Look into each cluster
c <- 3

cluster <- subset(hc_Blogs_df,hc_Blogs_df$hc_Blogs == c)

Words <- as.data.frame(unlist(Token_Blogs_nsw[as.integer(row.names(cluster))]))
colnames(Words) <- "Words"

Freq <- sqldf("SELECT Words W, COUNT(Words) Words FROM Words GROUP BY Words")
Freq_bar <- Freq[Freq$Words > nrow(cluster)*0.1,]


ggplot(Freq_bar, aes(x = reorder(W, -Words), y = Words)) + geom_bar(stat="identity") + xlab("Words") + ylab("Freq") + ggtitle(paste("Frequency chart Cluster",c,"(Size[",nrow(cluster),"])",sep = " ")) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



