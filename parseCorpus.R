library(tm)
library(stringr)
library(stylo)
library(quanteda)
library(splitstackshape)
library(slam)

numberOfSplits <- 10
maxN <- 5
ngrams <- 1:maxN
tdm.local.bounds <- c(2,Inf)
tdm.global.bounds <- c(1,Inf)
options(mc.cores=8)


splitsPerFile <- round(numberOfSplits / 3)
dirName <- paste0("f-",numberOfSplits)
textDir <- paste0("/data/",dirName)
column.names <- c()
for (i in 1:1000) {
  column.names <- c(column.names,paste0("t",i))  
}





LOG <- function(...)  {
  cat(paste(Sys.time())," : ",...,"\n",file="progress.log",append = T) 
}


my_tokenize <- function(x) {
  LOG(paste(x$meta$id," / ",splitsPerFile))
  terms <- c()
  for (ngram in ngrams) {
    ng <- make.ngrams(txt.to.words(x),ngram)
    terms <- c(terms,ng)
  }
  terms
}




splitFiles <- function() {
  LOG("split files")
  
  system(paste0("rm -rf /data/",dirName))
  system(paste0("mkdir /data/",dirName))
  system(paste0("cat /data/final/en_US/en_US.blogs.txt  | split -n r/",splitsPerFile,"  -d - /data/",dirName,"/blogs"))
  system(paste0("cat /data/final/en_US/en_US.news.txt  | split -n r/",splitsPerFile,"  -d - /data/",dirName,"/news"))
  system(paste0("cat /data/final/en_US/en_US.twitter.txt  | split -n r/",splitsPerFile,"  -d - /data/",dirName,"/twitter"))
}

createCorpus <- function(pattern="*") {
  LOG("create corpus")
  
  corpus <- Corpus(DirSource(textDir, encoding="UTF-8",pattern=pattern), readerControl = list(language="en_US"))
  cleanCorpus(corpus)
}

cleanCorpus <- function(corpus) {
  LOG("clean corpus")
  LOG("remove punctation")
  corpus <- tm_map(corpus,removePunctuation)
  LOG("remove numbers")
  corpus <- tm_map(corpus,removeNumbers)
  LOG("remove non-ascii")
  (f <- content_transformer(function(x) str_replace_all(x,"[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", " ")))
  corpus <- tm_map(corpus, f)
  LOG("remove whitespace")
  corpus <- tm_map(corpus,stripWhitespace)
  LOG("fix encoding")
  (g <- content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  corpus <- tm_map(corpus, g)
  LOG("transform tolower")
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus   
}


createTdm <- function(corpus) {
  LOG("create tdm")
  tdm <- TermDocumentMatrix(corpus,control=list(tokenize=my_tokenize,tolower=F,
                                                bounds=list(local=tdm.local.bounds,global=tdm.global.bounds),
                                                wordLengths=c(1,Inf)))
  tdm  
}


createDT <- function(tdm) {
  LOG("create DT")
  counts <- row_sums(tdm)
  dt <- data.table(names(counts))
  LOG("create DTS")
  dts <- cSplit(dt,"V1"," ")
  dts$count <- unname(counts)
  setnames(dts,c(column.names[1:maxN],"count"))
  rm(dt,counts)
  gc()
  
  for (i in seq_along(dts)) { 
    set(dts, i=which(is.na(dts[[i]])), j=i, value="")
  }
  setkeyv(dts,column.names[1:maxN])
  dts
}

  
