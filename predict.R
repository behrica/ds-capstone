library(tau)
library(data.table)


column.names <- c()
for (i in 1:1000) {
  column.names <- c(column.names,paste0("t",i))  
}

predictNextFromWords <- function(dts,words) {
  by <- column.names[min(ncol(dts)-1,length(words)+1)]
  allMatches <- dts[as.list(words),j=sum(count),by=by]
  allMatches <- allMatches[ allMatches[[by]] !="" ,]
  allMatches.sorted <- allMatches[order(V1,decreasing = T)]
  predictedWord <- as.character(allMatches.sorted[1][,by,with=FALSE][[1]])
  if (is.na(predictedWord)) {
    if (length(words)==1) predictedWord <-  "the"
    else predictedWord <- predictNextFromWords(dts,words[-1])
  } else {
    allMatches.sorted$prob <- allMatches.sorted[,V1] / sum(allMatches.sorted[,V1])
  }
  
  predictedWord
}

predictNext <- function(dts,phrase) {
  if (phrase=="") return("")
  words <- tokenize(phrase)
  words <- str_trim(words)
  words <- words[words!=""]
  words <- tail(words,ncol(dts)-2)
  predictNextFromWords(dts,words)
}
