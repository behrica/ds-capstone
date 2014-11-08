library(tm)
library(stringr)
library(markovchain)
library(ngram)
library(slam)
library(hash)




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
  }

tokenizeFile <- function(filename) {
  file <- file(filename)
   vs <- VectorSource(readLines(file))

    corpus <- Corpus(vs)


   tdm <- TermDocumentMatrix(corpus, control = list(
                                        removePunctuation=T,
                                        removeNumbers=T,
                                         tokenize = ngram_tokenizer,
                                         wordLengths=c(1,Inf)
   ))
  tdm$dimnames$Terms <- str_trim(tdm$dimnames$Terms)
  tdm
}




buildModel <- function(tdm) {

    freqs <- row_sums(tdm)
    terms <- remove_stopwords(str_trim(names(freqs)),words = c(""," "))

    name_pos <- hash(keys=terms,values = 1:length(terms))


    dtcm <- simple_triplet_diag_matrix(rep(1,length(terms)))

    for(term in terms) {

        words <- remove_stopwords(tokenize(term)," ")
        #print(words)
        l <- length(words)
        if (l<2)
            next


        pos.last.blank <- tail(str_locate_all(term," ")[[1]],1)[1]

        left <- str_trim(substring(term,pos.last.blank+1))
        right <- str_trim(substring(term,1,pos.last.blank-1))

        x <- values(name_pos,keys=right)
        y <- values(name_pos,keys=left)
        old <- as.matrix(dtcm[x,y])[1]
        dtcm[x,y] <- old + 1
       # print(paste(right,"-->",left,":",x,y,old+1))
                                        #handleTerm(term,dtcm);
    }
    dtcm
}



#tdm <-tokenizeFile("en_US.twitter.100.txt")
#dtcm <- buildModel(tdm)
