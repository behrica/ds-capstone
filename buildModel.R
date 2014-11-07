library(tm)
library(stringr)
library(markovchain)
library(ngram)

ngram_tokenizer <- function(x) {
    text <- str_trim(stripWhitespace(as.character(x)))

      ng1 <- get.ngrams(ngram(text,1))
      ng2 <- ng3 <- c()
      words <- length(ng1)
      if (words >=2)
          ng2 <- get.ngrams(ngram(text,2))
      if (words >=3)
          ng3 <- get.ngrams(ngram(text,3))
      terms  <- str_trim(c(ng1,ng2,ng3))
      terms
      #browser()
      #print(words)

  }

tokenizeFile <- function(filename) {
  file <- file(filename)
  #vs <- VectorSource(readChar(file, file.info(filename)$size))
   vs <- VectorSource(readLines(file))

    corpus <- Corpus(vs)


   tdm <- TermDocumentMatrix(corpus, control = list(
                                        removePunctuation=T,
                                        removeNumbers=T,
                                        tokenize = ngram_tokenizer
   ))
  tdm$dimnames$Terms <- str_trim(tdm$dimnames$Terms)
  tdm
}

tdm <-tokenizeFile("en_US.twitter.100.txt")
                                        #as.matrix(tdm)

terms <- Terms(tdm)
