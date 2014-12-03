library(tau)
library(data.table)
library(stringr)

column.names <- c()
for (i in 1:1000) {
  column.names <- c(column.names,paste0("t",i))
}

predictNextFromWords <- function(dts,words) {
  words <- tail(words,ncol(dts)-2)
  by <- column.names[min(ncol(dts)-1,length(words)+1)]
  allMatches <- dts[as.list(words),j=sum(count),by=by]
  allMatches <- allMatches[ allMatches[[by]] !="" ,]
  allMatches.sorted <- allMatches[order(V1,decreasing = T)]
  sum <- sum(allMatches.sorted$V1)
  allMatches.sorted$prob <- allMatches.sorted$V1 / sum
  #print(head(allMatches.sorted))

  predictedWord <- as.character(allMatches.sorted[1][,by,with=FALSE][[1]])
  if (is.na(predictedWord)) {
    prob <- 0
    if (length(words)==1) predictedWord <-  "the"
    else {
      prediction <- predictNextFromWords(dts,words[-1])
      predictedWord <- prediction$word
      prob <- prediction$prob
    }
  } else {
    prob <- round(allMatches.sorted$prob[1] * 100)
  }
  result = list(word=predictedWord, prob=prob)
}

predictNext <- function(dts,phrase) {
  if (phrase=="") return(list(word="", prob=0))
  words <- tokenize(phrase)
  words <- str_trim(words)
  words <- words[words!=""]
  words <- tail(words,ncol(dts)-2)
  words <- tolower(words)
  predictNextFromWords(dts,words)
}
