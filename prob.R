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
