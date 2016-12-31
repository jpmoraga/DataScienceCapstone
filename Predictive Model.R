library(tm)
library(qdapRegex)


setwd("/Users/jpmoraga/Desktop/Data Science Capstone/Predictive Model")

#################################################Functions############################################

# Create clean-up functions
cleanText = function(x){
  #   This simple function does not cover ambiguities such as 's or 'd
  x <- gsub("let's","let us",x)
  x <- gsub("I'm","I am",x)
  x <- gsub("'re", " are",x)
  x <- gsub("n't", " not",x)
  x <- gsub("'ll", " will",x)
  x <- gsub("'ve"," have",x)
  x <- gsub("â€™|â€œ|â€", "", x)
  x <- gsub("[^a-zA-Z ]", "", x)
  return(x)
}

## Remove stopwordsl
nsw = function(x){x[x %in% stopwords() == FALSE]}

## Extract n-grams form a string
extract_ngrams = function(x){
  
  x <- cleanText(x)  
  x <- rm_emoticon(x, trim = TRUE ,clean = TRUE)
  x <- tolower(x)
  
  if(length(strsplit(x,' ')[[1]]) == 1) {
    gram <- x
    return(gram)
  } else if(sapply(gregexpr("\\W+", x), length) + 1 == 2) {
    x <- strsplit(x, " ")
    gram <- x[[1]][1]
    bigram <- x[[1]][2]
    return(rbind(gram, bigram))
  } else if(sapply(gregexpr("\\W+", x), length) + 1 == 3) {
    x <- strsplit(x, " ")
    gram <- x[[1]][1]
    bigram <- x[[1]][2]
    trigram <- x[[1]][3]
    return(rbind(gram, bigram, trigram))
  } else {
    words <- sapply(x, wordpunct_tokenizer)
    x <- String(x)
    x <- x[words]
    gram <- x[[1]][length(x[[1]])]
    bigram <- paste(x[[1]][length(x[[1]])-1],x[[1]][length(x[[1]])],sep = " ")
    trigram <- paste(x[[1]][length(x[[1]])-2], x[[1]][length(x[[1]])-1],x[[1]][length(x[[1]])],sep = " ")
    fourgram <- paste(x[[1]][length(x[[1]])-3], x[[1]][length(x[[1]])-2], x[[1]][length(x[[1]])-1],x[[1]][length(x[[1]])],sep = " ")
    return(rbind(gram, bigram, trigram, fourgram))
  }
}

######################################################################################################

f1 <- read.table("/Users/jpmoraga/Desktop/Data Science Capstone/Predictive Model/Freq_Words.csv")
f2 <- read.table("/Users/jpmoraga/Desktop/Data Science Capstone/Predictive Model/Freq_Words2.csv")
f3 <- read.table("/Users/jpmoraga/Desktop/Data Science Capstone/Predictive Model/Freq_Words3.csv")
f4 <- read.table("/Users/jpmoraga/Desktop/Data Science Capstone/Predictive Model/Freq_Words4.csv")

f1 <- f1[f1$Words > 1,]
f2 <- f2[f2$Words > 1,]
f3 <- f3[f3$Words > 1,]
f4 <- f4[f4$Words > 1,]

f1$rank <- f1[,2]  
f2$rank <- f2[,2] + f1[1,2]
f3$rank <- f3[,2] + f2[1,2] + f1[1,2]
f4$rank <- f4[,2] + f3[1,2] + f2[1,2] + f1[1,2]

f1$nw <- 1
f2$nw <- 2
f3$nw <- 3
f4$nw <- 4

f1$pred <- ""

f2p <- as.data.frame(lapply(f2$W, extract_ngrams))
f2p <- t(f2p)
row.names(f2p) <- NULL
f2p <- f2p[,2]
f2$pred <- f2p

f3p <- as.data.frame(lapply(f3$W, extract_ngrams))
f3p <- t(f3p)
row.names(f3p) <- NULL
f3p <- f3p[,3]
f3$pred <- f3p

f4p <- as.data.frame(lapply(f4$W, extract_ngrams))
f4p <- t(f4p)
row.names(f4p) <- NULL
f4p <- f4p[,1]
f4$pred <- f4p

f <- rbind(f4,f3,f2,f1)

filepath <- "/Users/jpmoraga/Desktop/Data Science Capstone/Predictive Model"
write.table(f, paste(filepath,"f.csv",sep = "/"))

################################### Prediction #############################################
f <- read.table("/Users/jpmoraga/Desktop/Data Science Capstone/Predictive Model/Shiny_Capstone/f.csv")

f_1 <- head(f[f$nw == 2,])

y <- strsplit("This is a "," ")

y <- extract_ngrams(y)

g2 <- f[grep(paste0(paste0("^",y[1])," "), f$W),]
g2 <- g2[g2$nw == 2,]
g2 <- head(g2)
g2 <- as.data.frame(g2)

g3 <- f[grep(paste0(paste0("^",y[2])," "), f$W),]
g3 <- g3[g3$nw == 3,]
g3 <- head(g3)
g3 <- as.data.frame(g3)

g4 <- f[grep(paste0(paste0("^",y[3])," "), f$W),]
g4 <- g4[g4$nw == 4,]
g4 <- head(g4)
g4 <- as.data.frame(g4)

predf <- c(as.character(g4$pred),as.character(g3$pred),as.character(g2$pred))
predf <- as.data.frame(predf)
if(nrow(predf) == 0) {predf <- unique(f_1$pred)}
predf <- as.data.frame(predf)
predf <- aggregate(predf, by = list(predf$predf), FUN = length)
predf <- predf[order(-predf$predf),]

as.data.frame(predf[,1])

