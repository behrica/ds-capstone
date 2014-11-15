library(tm)
library(quanteda)
library(stringr)
library(slam)
library(hash)
library(stylo)

my_tokenize <- function(x,n=3) {
  text <- as.character(x)
  text <- text[length(tokenize(text,simplify = T))>=n]
  # if (text=="")
  #  return(c(""))
  ngrams <- unlist(ngrams(text,concatenator = " ",n,include.all = F))
  iconv((str_trim(ngrams)), "UTF-8", "UTF-8",sub='')
}

my_tokenize_2 <- function(x) {
  ng3 <- make.ngrams(txt.to.words(x),3)
  ng4 <- make.ngrams(txt.to.words(x),4)
  c(ng3,ng4)
  
}


#corpus <- readRDS("cleanCorpus.rds")
options(mc.cores=8);tdm <- TermDocumentMatrix(corpus,control=list(tokenize=my_tokenize_2,tolower=F,bounds=list(local=c(2,Inf))))
terms <- row_sums(tdm)
sum.hash <- hash(terms)


 predict <- function(tdm,term) {
  matches <- grep(paste0(term," ([a-z])*"),Terms(tdm))
  if (length(matches) == 0) {
    words <- tokenize(term,simplify=T)
    if (length(words) == 1 )
      return("the")
    predict(tdm,paste(words[-1],collapse=" " ))
  }
    
  else  
  sort(rowSums(as.matrix(tdm[matches,])),decreasing = T)[1]
}

# <<TermDocumentMatrix (terms: 4128659, documents: 100)  n=1,2,3,4   size =710M
# <<TermDocumentMatrix (terms: 44905127, documents: 4272) n=3 size 4.4 GB
# <<TermDocumentMatrix (terms: 333810, documents: 4272)>> n=3, bounds(2,Inf)
#<<TermDocumentMatrix (terms: 482749, documents: 4272)>> n=3,4 bounds(2,Inf)