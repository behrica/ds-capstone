library(tm)
library(ngram)
library(qdapTools)
library(stringr)
library(parallel)
library(slam)

my_tokenize <- function(x) {
  text <- concat(x)
  gram2 <- get.ngrams(ngram(text,2))
  gram3 <- get.ngrams(ngram(text,3))
  gram4 <- get.ngrams(ngram(text,4))
  iconv((str_trim(c(gram2,gram3,gram4))), "UTF-8", "UTF-8",sub='')
}

options(mc.cores=8)
print(paste("start:",Sys.time()))
filePattern <- "news.split*"
#filePattern <- "*"
corpus <- Corpus(DirSource("final//en_US.split", encoding="UTF-8",pattern=filePattern), readerControl = list(language="en_US"))
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
(f <- content_transformer(function(x) str_replace_all(x,"[^[:alnum:]]", " ")))
corpus <- tm_map(corpus, f)
corpus <- tm_map(corpus,stripWhitespace)
(g <- content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
corpus <- tm_map(corpus, g)
corpus <- tm_map(corpus, content_transformer(tolower))
tdm <- TermDocumentMatrix(corpus,control=list(tokenize=my_tokenize,tolower=F))
#terms <- row_sums(tdm)
#sum.hash <- hash(data.frame(term=names(terms),count=terms))
print(paste("end:",Sys.time()))



