library(tm)
library(quanteda)
library(stringr)
library(slam)
library(hash)
library(stylo)
library(slam)

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

my_tokenize_3 <- function(x) {
  ng3 <- make.ngrams(txt.to.words(x),3)
  ng4 <- make.ngrams(txt.to.words(x),4)
  ng5 <- make.ngrams(txt.to.words(x),5)
 c(ng3,ng4,ng5)
}

my_tokenize_4 <- function(x) {
  make.ngrams(txt.to.words(x),2)
}



#corpus <- readRDS("cleanCorpus.rds")
options(mc.cores=8);tdm <- TermDocumentMatrix(corpus,control=list(tokenize=my_tokenize_2,tolower=F,bounds=list(local=c(1,Inf))))
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

# <<TermDocumentMatrix (terms: 4128659, documents: 100)  n=1,2,3,4   size =710M .local.bounds(1,Inf)  #
# <<TermDocumentMatrix (terms: 44905127, documents: 4272) n=3 size 4.4 GB .local.bounds(1,Inf) 
# <<TermDocumentMatrix (terms: 333810, documents: 4272)>> n=3, .local.bounds(2,Inf)  
# <<TermDocumentMatrix (terms: 482749, documents: 4272)>> n=3,4 .local.bounds(2,Inf)  
# <<TermDocumentMatrix (terms: 540961, documents: 4272)>>  n = 3,4,5 .local.bounds(2,Inf) 
# <<TermDocumentMatrix (terms: 15743504, documents: 4272)>> n=2   .local.bounds(1,Inf) 
# <<TermDocumentMatrix (terms: 51103022, documents: 99)>>   n=2,3 size=4.8 G .local. c(1,Inf) 
# <<TermDocumentMatrix (terms: 3390046, documents: 99)>>  n=2,3,4,5 size=500 M
findTerms <- function(tdm,prefix) {
  terms <- row_sums(tdm)
  data.frame(count=sort(terms[grep(prefix,names(terms),value=F)],decreasing = T))
}

showLikelyHoods <- function(tdm,term,options) {
  terms <- row_sums(tdm)
  counts = c()
  for (option in options) {
    matches <-grep(paste0("^",term," ",option,"$"),names(terms),value=F)
    print(paste(term,option,terms[matches]))
    counts[[option]] <- terms[matches]
  }
  counts
}
  
