library(tm)
library(ngram)
library(slam)



options(mc.cores=1)
filePattern <- "*"
corpus <- Corpus(DirSource("final//en_US.split", encoding="UTF-8",pattern=filePattern), readerControl = list(language="en_US"))
corpus1 <- tm_map(corpus,removePunctuation)
corpus2 <- tm_map(corpus1,removeNumbers)
(f <- content_transformer(function(x) str_replace_all(x,"[^[:alnum:]]", " ")))
corpus3 <- tm_map(corpus2, f)
corpus4 <- tm_map(corpus3,stripWhitespace)
(g <- content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
corpus5 <- tm_map(corpus4, g)
corpus6 <- tm_map(corpus5, content_transformer(tolower))

saveRDS(corpus6, "cleanCorpus.rds")

