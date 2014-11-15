
library(tm)
library(stringr)
library(markovchain)
library(ngram)
library(slam)
library(hash)


tokenize_ngrams <- function(x, n=3) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))

my.read.lines2=function(fname) {
 s = file.info( fname )$size
 buf = readChar( fname, s, useBytes=T)
 strsplit( buf,"\n",fixed=T,useBytes=T)[[1]]
}

lineCount <- 0

tokenize_line <- function(line) {
    lineCount <<- lineCount + 1
    if (line=="")
        return("")

    ng1 <- get.ngrams(ngram(line,1))
    ng2 <- ng3 <- ng4 <- c()
    words <- length(ng1)
    if (words >=2)
        ng2 <- get.ngrams(ngram(line,2))
    if (words >=3)
        ng3 <- get.ngrams(ngram(line,3))
    if (words >=4)
        ng4 <- get.ngrams(ngram(line,4))
    terms  <- c(ng1,ng2,ng3,ng4)
    if(rbinom(1,1,0.001)==1)
        print(paste(Sys.time(),"#lc:",lineCount))
    terms
}
ngram_tokenizer <- function(x) {
    print(paste(Sys.time(),": start handle document: ",meta(x),"with lines: ",length(x$content)))

     print(paste(Sys.time(),": tolower"))
     text <- tolower(x)

     print(paste(Sys.time(),": removePunctuation"))
     text <- removePunctuation(text)

     print(paste(Sys.time(),": removeNumbers"))
     text <- removeNumbers(text)

     print(paste(Sys.time(),": stripWhiteSpace"))
     text <- stripWhitespace(text)

     print(paste(Sys.time(),": trim"))
     text <- str_trim(text)

     print(paste(Sys.time(),": tokenize"))
    unlist(lapply(text,FUN = tokenize_line))

  }

tokenizeFile <- function(filename) {
  print(paste(Sys.time(),": start"))

  lineCount <<- 0
  vs <- URISource(paste0("file://",filename))
  corpus <- Corpus(vs)
  tdm <- TermDocumentMatrix(corpus, control = list(
    bounds=list(global=c(2,Inf)),
                                        tokenize = ngram_tokenizer,
                                        stopwords = c(""," "),
                                        wordLengths=c(1,Inf)
   ))
   tdm
  #print(paste("Hash tdm: ",nTerm(tdm), "terms"))
  #hash(Terms(tdm),tdm$v)
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

calculateProb<- function(ngramCounts,phrase) {
    words <- str_split(phrase," ")[[1]]
    prev <- words[2]
    c_phrase <- ngramCounts[[phrase]]
    c_prev <- ngramCounts[[prev]]
    c_phrase / c_prev

}

predictNextWord <- function(ngramCounts,words) {
    keys <- keys(ngramCounts)
    matched <- grep(paste0("(^| )",words," (.*)"),keys,value = T)
    matched.probs <- sapply(matched, function(x) calculateProb(ngramCounts,x))
    max.ind <- which.max(matched.probs)
    matched.probs[max.ind]
    #matches <- grep(paste0(phrase," .*"),names(sums))
    #matchedTerm <- names(which.max(sums[matches]))
    #regmatches(matchedTerm,regexec(paste0(".* |^",phrase," (.*)"),matchedTerm))
    #[[1]][2]
    #matchedTerm
}

#tdm <-tokenizeFile("en_US.twitter.10000.txt")

#dtcm <- buildModel(tdm)
