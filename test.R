library(RCurl)
library(tm)
library(microbenchmark)
source("parseCorpus.R")
source("predict.R")

#test.text <- getURLContent("http://www.gutenberg.org/cache/epub/47448/pg47448.txt")
#writeLines(test.text,"test.txt")



# predictNext <- function(dts,phrase) {
#   return("the")
# } 
# 
# cleanCorpus <- function(corpus) {
#   return(corpus)
# }


readTestWords <- function() {
  corpus <- Corpus(URISource("file://test.txt"))
  corpus <- cleanCorpus(corpus)
  c  <- content(corpus)[[1]]
  words <- words(c)
}


testAccuracy <- function(dts,words){
  word.count=length(words)-11
  correct <- 0
  predictions <- 0
  for (i in 1:word.count) {
    print(paste("word:",i,"/",word.count))
    for (j in 0:10) {
      ngram <- paste(words[i:(i+j)],collapse =" ")
      predicted <- predictNext(dts,ngram)
      predictions <- predictions + 1
      if (predicted == words[i+j+1]) {
        correct <- correct + 1
        print(paste("                   Succes: ",correct / predictions * 100))
      }
    }
  }
  worstCase <- paste(rep("Ü",10),collapse = " ")
  microbenchmark(predictNext(dts,worstCase))
}






