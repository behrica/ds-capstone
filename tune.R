library(tm)
library(stringr)
library(stylo)
library(quanteda)
library(splitstackshape)

numberOfSplits <- 100
ngrams <- c(2,3,4,5)
tdm.local.bound <- c(2,Inf)


#---------------------

splitsPerFile <- round(numberOfSplits / 3)


my_tokenize <- function(x) {
  cat(x$meta$id," / ",splitsPerFile,"\n")
  terms <- c()
  for (ngram in ngrams) {
    ng <- make.ngrams(txt.to.words(x),ngram)
    terms <- c(terms,ng)
  }
  terms
}




dirName <- paste0("f-",numberOfSplits)
system(paste0("rm -rf /mnt/data/",dirName))

system(paste0("mkdir /mnt/data/",dirName))

system(paste0("cat /mnt/data/final/en_US/en_US.blogs.txt  | split -n r/",splitsPerFile,"  -d - /mnt/data/",dirName,"/blogs"))
system(paste0("cat /mnt/data/final/en_US/en_US.news.txt  | split -n r/",splitsPerFile,"  -d - /mnt/data/",dirName,"/news"))
system(paste0("cat /mnt/data/final/en_US/en_US.twitter.txt  | split -n r/",splitsPerFile,"  -d - /mnt/data/",dirName,"/twitter"))

options(mc.cores=1)
textDir <- paste0("/mnt/data/",dirName)

corpus <- Corpus(DirSource(textDir, encoding="UTF-8",pattern="*"), readerControl = list(language="en_US"))
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
(f <- content_transformer(function(x) str_replace_all(x,"[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", " ")))
corpus <- tm_map(corpus, f)
corpus <- tm_map(corpus,stripWhitespace)
(g <- content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
corpus <- tm_map(corpus, g)
corpus <- tm_map(corpus, content_transformer(tolower))

options(mc.cores=7)
tdm <- TermDocumentMatrix(corpus,control=list(tokenize=my_tokenize,tolower=F,bounds=list(local=tdm.local.bound)))
counts <- row_sums(tdm)


dt <- as.data.table(names(counts))
dts <- cSplit(dt,"V1"," ")
dts$count <- unname(counts)
setnames(dts,c("t1","t2","t3","t4","t5","count"))
rm(dt,counts)
setkeyv(dts,c("t1","t2","t3","t4","t5"))

# predict
match <- (dts[list("i","want")])[is.na(t4)]
match[order(count,decreasing = T)][2]

# predict
match <- (dts[list("my","name","is")])[is.na(t5)]
match[order(count,decreasing = T)][2]



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
  