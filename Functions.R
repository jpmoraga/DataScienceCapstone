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


## Cosine metric
cos_dist = function(x,y)
     {
      1 - sum(x * y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
}

## Extract n-grams form a string
extract_ngrams = function(x){

  x <- cleanText(x)  
  x <- rm_emoticon(x, trim = TRUE ,clean = TRUE)
  x <- tolower(x)
  
  if(sapply(gregexpr("\\W+", x), length) + 1 == 1) {
    gram <- x
  } else if(sapply(gregexpr("\\W+", x), length) + 1 == 2) {
    x <- strsplit(x, " ")
    gram <- x[[1]][1]
    bigram <- x[[1]][2]
  } else {
    words <- sapply(x, wordpunct_tokenizer)
    x <- String(x)
    x <- x[words]
    gram <- x[[1]][length(x[[1]])]
    bigram <- paste(x[[1]][length(x[[1]])-1],x[[1]][length(x[[1]])],sep = " ")
    ## trigram <- paste(x[[1]][length(x[[1]])-2], x[[1]][length(x[[1]])-1],x[[1]][length(x[[1]])],sep = " ")
    return(rbind(gram, bigram))
  }
}