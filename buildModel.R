library(tm)
library(stringr)
library(markovchain)
library(ngram)
library(slam)
library(hash)

my.read.lines2=function(fname) {
 s = file.info( fname )$size
 buf = readChar( fname, s, useBytes=T)
 strsplit( buf,"\n",fixed=T,useBytes=T)[[1]]
}


tokenize_line <- function(line) {
          #print(line)
          ng1 <- get.ngrams(ngram(line,1))
          ng2 <- ng3 <- c()
          words <- length(ng1)
          if (words >=2)
            ng2 <- get.ngrams(ngram(line,2))
          if (words >=3)
           ng3 <- get.ngrams(ngram(line,3))
         terms  <- c(ng1,ng2,ng3)
         if(rbinom(1,1,0.001)==1)
             print(paste(Sys.time(),"#terms:",length(terms)))
          #print(terms)
          terms
}

ngram_tokenizer <- function(x) {
    text <- tolower(x)
    text <- removePunctuation(text)
    text <- removeNumbers(text)
    text <- stripWhitespace(text)
    text <- str_trim(text)

      unlist(lapply(text,FUN = tokenize_line))

  }

tokenizeFile <- function(filename) {
  vs <- URISource(paste0("file://",filename))
  corpus <- Corpus(vs)
  tdm <- TermDocumentMatrix(corpus, control = list(
                                        tokenize = ngram_tokenizer,
                                        stopwords = c(""," "),
                                        wordLengths=c(1,Inf)
   ))
  #tdm$dimnames$Terms <- stripWhitespace(str_trim(tdm$dimnames$Terms))
  tdm
#  sums <- row_sums(tdm)
 #sums
}




buildModel <- function(tdm) {

    freqs <- row_sums(tdm)
    terms <- sort(remove_stopwords(str_trim(names(freqs)),words = c(""," ")))

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

calculateProb <- function(tdm,phrase) {
    words <- str_split_fixed(phrase," ",2)[1]
    prev <- words[2]
    c_phrase <- tdm[which(Terms(tdm)==phrase),]$v
    c_prev <- tdm[which(Terms(tdm)=="can"),]$v
    c_phrase / c_prev

}

predictNextWord <- function(tdm,words) {
    m <- as.matrix(tdm[grep(paste0("^ ",words," (.*)"),Terms(tdm)),])
    max.ind <-  which.max(m)
    dimnames(m)$Terms[max.ind]

    #matches <- grep(paste0(phrase," .*"),names(sums))
    #matchedTerm <- names(which.max(sums[matches]))
    #regmatches(matchedTerm,regexec(paste0(".* |^",phrase," (.*)"),matchedTerm))
    #[[1]][2]
    #matchedTerm
}

#tdm <-tokenizeFile("en_US.twitter.10000.txt")

#dtcm <- buildModel(tdm)
