library(tm)
library(stringr)
library(stylo)
library(quanteda)
library(splitstackshape)
library(slam)

numberOfSplits <- 100
maxN <- 5
ngrams <- c(1,2,3,4,maxN)
tdm.local.bound <- c(2,Inf)


#---------------------

splitsPerFile <- round(numberOfSplits / 3)
dirName <- paste0("f-",numberOfSplits)
textDir <- paste0("/data/",dirName)


my_tokenize <- function(x) {
  message(x$meta$id," / ",splitsPerFile)
  terms <- c()
  for (ngram in ngrams) {
    ng <- make.ngrams(txt.to.words(x),ngram)
    terms <- c(terms,ng)
  }
  terms
}



system(paste0("rm -rf /data/",dirName))
system(paste0("mkdir /data/",dirName))
system(paste0("cat /data/final/en_US/en_US.blogs.txt  | split -n r/",splitsPerFile,"  -d - /data/",dirName,"/blogs"))
system(paste0("cat /data/final/en_US/en_US.news.txt  | split -n r/",splitsPerFile,"  -d - /data/",dirName,"/news"))
system(paste0("cat /data/final/en_US/en_US.twitter.txt  | split -n r/",splitsPerFile,"  -d - /data/",dirName,"/twitter"))

options(mc.cores=4)
corpus <- Corpus(DirSource(textDir, encoding="UTF-8",pattern="*00"), readerControl = list(language="en_US"))
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
(f <- content_transformer(function(x) str_replace_all(x,"[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", " ")))
corpus <- tm_map(corpus, f)
corpus <- tm_map(corpus,stripWhitespace)
(g <- content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
corpus <- tm_map(corpus, g)
corpus <- tm_map(corpus, content_transformer(tolower))

options(mc.cores=4)
tdm <- TermDocumentMatrix(corpus,control=list(tokenize=my_tokenize,tolower=F,bounds=list(local=tdm.local.bound),wordLengths=c(1,Inf)))
counts <- row_sums(tdm)


dt <- data.table(names(counts))
dts <- cSplit(dt,"V1"," ")
dts$count <- unname(counts)
setnames(dts,c("t1","t2","t3","t4","t5","count"))
rm(dt,counts)

for (i in seq_along(dts)) { 
  set(dts, i=which(is.na(dts[[i]])), j=i, value="")
}
setkeyv(dts,c("t1","t2","t3","t4","t5"))

# predict
#match <- (dts[list("i","want")])[is.na(t4)]
#match[order(count,decreasing = T)][2]

# predict
#match <- (dts[list("my","name","is")])[is.na(t5)]
#match[order(count,decreasing = T)][2]



predictNext <- function(tdm,term) {
  matches <- grep(paste0(term," ([a-z])*"),Terms(tdm))
  if (length(matches) == 0) {
    words <- tokenize(term,simplify=T)
    if (length(words) == 1 )
      return("the")
    predictNext(tdm,paste(words[-1],collapse=" " ))
  }
  
  else  {
    mostProbableWords <- sort(row_sums(tdm[matches,]),decreasing = T)
    head(mostProbableWords)
  }  
}
  
calcProbSingle <- function(dts,words) {
  words.padded <- c(words,rep("",100))[1:maxN]
  matched <- dts[as.list(words.padded),nomatch=0]
  count.phrase <- matched$count
  
  words <- words[-1]
  words.padded <- c(words,rep("",100))[1:maxN]
  
  
  matched <- dts[as.list(words.padded),nomatch=0]
  count.phrase_1 <- matched$count  
  #if (is.na(count.phrase) | is.na(count.phrase_1)) 
    #0
  if (length(count.phrase) == 0 | length(count.phrase_1) == 0)
    return(0)
  count.phrase / count.phrase_1  
}

calcProb <- function(dts,phrase) {
  words <- tokenize(phrase,simplify=T)
  words <- words[max(1,(length(words)-maxN+1)):length(words)]
  
  while(TRUE) {
    print(words)
    prob <- calcProbSingle(dts,words)
    words <- words[-1]
    if (prob > 0 | length(words)==0)
      return(prob)
  }
  
}
