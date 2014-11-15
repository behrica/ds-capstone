library(data.table)
library(tau)
library(ngram)

my.read.lines2=function(fname) {
  s = file.info( fname )$size
  s = 20000000
  
  buf = readChar( fname, s)
  lines <- strsplit(buf,"\n",fixed=T)[[1]]
}

extractNgrams <- function(x,n) {
  textcnt(x,n=n,verbose = T,method = "string",decreasing = T)
}

extractCounts <- function(files) {
  results = list()
  for (file in files) {
    text <- my.read.lines2(file)
    cnt_1 <- extractNgrams(text,1)
    cnt_2 <- extractNgrams(text,2)
    cnt_3 <- extractNgrams(text,3)
    cnt_4 <- extractNgrams(text,4)    
    results[[file]] <- list(cnt_1,cnt_2,cnt_3,cnt_4)
  }
  results
}

