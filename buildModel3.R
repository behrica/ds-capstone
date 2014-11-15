library(pander)
library(data.table)
library(tau)
library(ngram)

my.read.lines2=function(fname,maxChar) {
  s = file.info( fname )$size
  buf = readChar( fname, maxChar)
  lines <- strsplit(buf,"\n",fixed=T)[[1]]
}

extractNgrams <- function(x,n) {
  textcnt(x,n=n,method = "string",decreasing = T)
}

extractCounts <- function(files,maxChar) {
  results = list()
  for (file in files) {
    text <- my.read.lines2(file,maxChar)
    cnt_1 <- extractNgrams(text,1)
    cnt_2 <- extractNgrams(text,2)
    cnt_3 <- extractNgrams(text,3)
    cnt_4 <- extractNgrams(text,4)    
    results[[file]] <- list(gram1 = cnt_1,gram2=cnt_2,gram3=cnt_3,gram4=cnt_4)
  }
  results
}

tokenizeCorpus <- function(maxChar) {
 extractCounts(c("final//en_US/en_US.blogs.txt","final//en_US/en_US.news.txt","final//en_US/en_US.twitter.txt"),maxChar)
}



