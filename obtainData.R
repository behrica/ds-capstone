library(hash)
library(dplyr)
library(tm)
library(RWeka)
library(tau)
library(qdapDictionaries)

#download.file("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip")
#unzip("Coursera-SwiftKey.zip")

writeSample <- function() {

    con <- file("final/en_US/en_US.twitter.txt")
    all <- readLines(con)
    s100 <- sample(all,100)
    writeLines(s100,"en_US.twitter.100.txt")
    close(con)
}


tokenizeFile <- function(filename) {
  file <- file(filename)
  vs <- VectorSource(readChar(file, file.info(file)$size))
    #vs <- VectorSource(readLines(file))

    corpus <- Corpus(vs)

    tokenizer <- WordTokenizer
      #function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer,removePunctuation=T,remove_stopwords=T))
}




#writeSample()
tdm <-tokenizeFile("en_US.twitter.10000.txt")
m <- as.matrix(tdm)

numWords <- length(m)
percent_unique <- length(m[m==1,]) / numWords

plot(sapply(1:max(m),function(x) length(m[m>x,]) ))



terms <- Terms(tdm)
foreign1 <- setdiff(terms,GradyAugmented)
foreign2 <- grep("[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]",terms,value=T)

